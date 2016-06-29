(ns strictly-specking.parse-spec
  (:require
   [clojure.set :as set]
   [clojure.spec :as s]))

  ;; we are dealing with a datatype
  ;; a list of paths Monoid
  ;; mempty   []
  ;; mappend  cons
  ;; mconcat  concat

  ;; its also join and intersection
  ;; join         set/union
  ;; intersection set/intersection

  ;; we need to cons a value to the head of each of the lists
  ;; map-cons v list-of-paths 

;;

(defn spec-from-registry [spec-key]
  (get (s/registry) spec-key))

(defn parse-keys-args [& {:keys [req opt req-un opt-un]}]
  (let [spec-specs  (into (vec req) opt)
        un-specs    (into (vec req-un) opt-un)
        known-keys  (into spec-specs (mapv #(-> % name keyword) un-specs))
        keys->specs (zipmap known-keys (into spec-specs un-specs))]
    {:keys->specs keys->specs
     :k->s        #(or (keys->specs %) %)
     :known-keys  (set known-keys)}))


(declare poss-path)

(def empty-path-set #{})
(def consable-empty-path-set #{[]})

;; a lifting operation
;; f -> PathSet -> PathSet
(defn path-set-fmap [f path-set]
  (into #{} (mapv f path-set)))

(defn consable-path-set [path-set]
  (or (not-empty path-set) consable-empty-path-set))

(defn path-set-cons [v path-set]
  (path-set-fmap #(cons v %) path-set #_(consable-path-set path-set)))

;; [PathSet] -> PathSet 
(defn path-set-join [list-of-path-sets]
  (apply set/union list-of-path-sets))

;; [PathSet] -> PathSet
(defn path-set-intersect [list-of-path-sets]
  (apply set/intersection list-of-path-sets))

(defn key-paths [f desc]
  {:pre [(sequential? desc)]}
  (let [{:keys [known-keys k->s]} (apply parse-keys-args (rest desc))]
    (path-set-join
     (map #(path-set-cons (hash-map :ky % :ky-spec (k->s %)) (poss-path f (k->s %)))
      known-keys))))

;; DecribeKeyedVals -> [DecribeVal]
(defn desc-keyed-vals [desc]
  (vals (apply hash-map (rest desc))))

;; DescribeVals -> [DescribeVal] 
(def desc-vals rest)

(def regex-ops #{'* '+ '? 'alt 'cat '&})

(defn is-regex? [desc]
  (or (and (sequential? desc)
           (regex-ops (first desc)))
      (and (keyword? desc)
           (when-let [spec (spec-from-registry desc)]
             (s/regex? spec)))
      (s/regex? desc)))

(def int-key {:ky :strictly-specking.core/int-key})

(def int-key? #(= :strictly-specking.core/int-key (:ky %)))

(defn regex-poss-path [f desc]
  (let [path-set (poss-path f desc)]
    (if (is-regex? desc)
      ;; remove ::int-key from beginning of the paths
      (path-set-fmap #(if (int-key? (first %)) (rest %) %)
                     path-set)
      path-set)))

(defn expanded-map-of-desc? [x]
  (and (sequential? x)
       (= (first x) 'every)
       (even? (count (drop 2 x)))
       (= 'map?
          (:clojure.spec/kind-form
           (apply hash-map (drop 2 x))))))

(defn handle-expanded-map-of [f desc]
  (when-let [[ky-pred val-pred] (expanded-map-of-desc? desc)]
    (poss-path f (list 'map-of ky-pred val-pred))))

;; doesn't handle recursion
;; this can be refactored into a more general movement through
;; the structure
;; can at the very least make sure this is lazy and we can then just grab the first couple of
;; paths

;; also need to make a suite of tests to verify that this is future proof
;; and that our expectations about spec descriptions hold firm



(defn poss-path [f desc]
  (if (and
       (keyword? desc)
       (spec-from-registry desc))
    (let [res (f desc)]
      (if (fn? res)
        (poss-path res (s/describe desc))
        res))
    ;;#{[{:end-ref desc }]}
    (let [poss-path       (partial poss-path f)
          regex-poss-path (partial regex-poss-path f)
          key-paths       (partial key-paths f)
          ]
      (condp = (and (sequential? desc)
                    (first desc))
        'keys        (key-paths desc)
        'strict-keys (key-paths desc)
        'or          (path-set-join (map poss-path
                                         (desc-keyed-vals desc)))
        ;; this is really the intersection of paths, but there is a notion of
        ;; a infinite path set so ...
        'and         (path-set-join (map poss-path (desc-vals desc)))
        'merge       (path-set-join (map poss-path (desc-vals desc)))
        
        '*     (path-set-cons int-key (regex-poss-path (second desc)))
        '+     (path-set-cons int-key (regex-poss-path (second desc)))
        '?     (path-set-cons int-key (regex-poss-path (second desc)))
        'alt   (path-set-cons int-key
                              (path-set-join (map regex-poss-path (desc-keyed-vals desc))))
        'cat   (path-set-cons int-key
                              (path-set-join (map regex-poss-path (desc-keyed-vals desc))))
        '&     (path-set-cons int-key (regex-poss-path (second desc)))
        ;; cat is a splicing operation something without a numeric path is going to get one consed on
        
        
        'tuple        (path-set-cons int-key
                                     (path-set-join (map poss-path (desc-vals desc))))
        'col-of       (path-set-cons int-key (poss-path (second desc)))
        'col-checker  (path-set-cons int-key (poss-path (second desc)))
        ;; need to check for every tuple
        ;; 
        'every  (or (handle-expanded-map-of desc)
                    (path-set-cons int-key (poss-path (second desc))))
        'map-of (let [key-predicate (second desc)]
                  (path-set-cons {:ky ::pred-key :ky-pred-desc key-predicate}
                                 (poss-path (last desc))))
        'spec   (poss-path (second desc))
        ;; 'cat   
        ;; else
        empty-path-set
        
        ))))

(defn prevent-recursion-find [{:keys [path-keys-seen to] :as state}]
  (fn [x]
    (cond
      (= x to) #{[]}
      (path-keys-seen x) #{[]}
      :else (prevent-recursion-find (update-in state [:path-keys-seen] conj x)))))

(defn find-key-path [from to]
  (poss-path
   ;; can prevent recursion here easily
   ;; but this monady approach makes me think that there is a better
   ;; structure to the above
   (prevent-recursion-find {:to to :path-keys-seen #{}})
   (s/describe from)))

#_(find-key-path :fig-opt/real :fig-opt/car1)


;; regex NOTE
;; so every regex has an ::int-key because they index into a sequence
;; 

;; all compond regex operators are splicing, meaning that they strip the :int-key off
;; of the sub expression if they are regex
;; this is the equivalent of concatenation something that was a sublist becomes part of the
;; super list

#_(poss-path (s/describe (s/or :hi (s/or :hey :fig-opt/real)
                                    :bye (s/or :son :fig-opt/car)
                                    :lei (s/or :son :fig-opt/house))))

#_(poss-path (s/describe (s/and ;:hi (s/or :hey :fig-opt/real)
                             (s/or :son :fig-opt/car)
                             (s/or :son :fig-opt/house))))

#_(= (poss-path (s/describe (s/and :fig-opt/house)))
     (poss-path (s/describe (s/or :son :fig-opt/house))))

#_(poss-path (s/describe :fig-opt/real))
