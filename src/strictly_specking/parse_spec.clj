(ns strictly-specking.parse-spec
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [strictly-specking.fuzzy :refer [similar-key]]))

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


(defn keys-specs->keys-args [keys->specs]
  {:keys->specs keys->specs
   :k->s        #(or (keys->specs %) %)
   :known-keys  (set (keys keys->specs))})

;; TODO the shape of parsed keys args is redundant and doesn't
;; capture required keys information
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
       (let [args (apply hash-map (drop 2 x))]
         (or
          (= {} (:into args))
          (= 'map? (:clojure.spec/kind-form args))))
       (rest (second x))))

(defn handle-expanded-map-of [f desc]
  (when-let [res (expanded-map-of-desc? desc)]
    (poss-path f (cons 'map-of res))))

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
    (let [poss-path       (partial poss-path f)
          regex-poss-path (partial regex-poss-path f)
          key-paths       (partial key-paths f)]
      (if-let [op (and (sequential? desc)
                       (first desc))]
        (condp = op
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
          'every  (or (handle-expanded-map-of f desc)
                      (path-set-cons int-key (poss-path (second desc))))
          'map-of (let [key-predicate (second desc)]
                    (path-set-cons {:ky :strictly-specking.core/pred-key
                                    :ky-pred-desc key-predicate}
                                   (poss-path (nth desc 2))))
          ;; TODO this shouldn't be here
          'non-empty-map-of (let [key-predicate (second desc)]
                    (path-set-cons {:ky :strictly-specking.core/pred-key
                                    :ky-pred-desc key-predicate}
                                   (poss-path (nth desc 2))))
          'every-kv (let [key-predicate (second desc)]
                      (path-set-cons {:ky :strictly-specking.core/pred-key
                                      :ky-pred-desc key-predicate}
                                     (poss-path (nth desc 2))))        
          'spec   (poss-path (second desc))
          ;; 'cat   
          ;; else
          (f empty-path-set))
        (f empty-path-set)))))

(defn empt [f]
  (fn [x]
    (if (= empty-path-set x)
      empty-path-set
      (f x))))

(defn possible-child-keys [ns-ky-or-spec]
  (set
   (map first
   (poss-path
    (empt
     (fn [x]
       (fn [y]
         #{[]})))
    (s/describe ns-ky-or-spec)))))

;; could keep a set for a faster check
(defn path-predicate [pred]
  ((fn path-monad [path]
     (empt
      (fn [k]
        (let [p (cons k path)]
          (cond
            ;; prevents infinite recursion
            ((set path) k) #{}
            (pred p) #{[]}
            :else (path-monad p))))))
   []))

#_(poss-path
   (path-predicate (constantly false))
   (s/describe :fig-opt/build-config))

#_(possible-child-keys :cljsbuild.lein-project.require-builds/cljsbuild)

#_(possible-child-keys :strictly-specking.test-schema/compiler)

#_(s/describe :strictly-specking.test-schema/compiler)

#_(s/describe :strictly-specking.cljs-options-schema/source-map)

#_(s/map-of keyword? (s/map-of keyword? (s/every keyword?)))

;; finding keys of a parent

(defn find-key-path-without-ns [from to]
  (poss-path
   (path-predicate #(= (name (first %)) (name to)))
   (s/describe from)))

;; this only works for the name part
(defn find-key-path-like-key [from to]
  (poss-path
   (path-predicate #(similar-key (keyword (name (first %)))
                                 to))
   (s/describe from)))

#_(find-key-path-without-ns :strictly-specking.test-schema/compiler
                          :source-map)

;; TODO this is a test
#_(find-key-path-without-ns :strictly-specking.test-schema/lein-project-with-cljsbuild
                            :figwheel)
;; result
#_ #{({:ky-spec :cljsbuild.lein-project.require-builds/cljsbuild, :ky :cljsbuild}
     {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
     {:ky :strictly-specking.core/pred-key,
      :ky-pred-desc :strictly-specking.test-schema/string-or-named}
     {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})
    ({:ky-spec :cljsbuild.lein-project.require-builds/cljsbuild, :ky :cljsbuild}
     {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
     {:ky :strictly-specking.core/int-key}
     {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})
    ({:ky-spec :figwheel.lein-project/figwheel, :ky :figwheel})}


#_(s/describe :strictly-specking.test-schema/builds)


#_(s/describe (s/every-kv keyword? ::s/any))

#_(find-key-path-without-ns :cljsbuild.lein-project.require-builds/cljsbuild :builds)

(defn find-key-path [from to]
  (poss-path
   ;; can prevent recursion here easily
   ;; but this monady approach makes me think that there is a better
   ;; structure to the above
   (path-predicate #(= to (first %)))
   (s/describe from)))

(defn spec-key-to-parsed-args [spec-key]
  (when-let [children (not-empty (possible-child-keys spec-key))]
    (let [keys->specs (into {} (map (juxt :ky :ky-spec) children))]
      {:keys->specs keys->specs
       :k->s        #(or (keys->specs %) %)
       :known-keys  (set (keys keys->specs))})))

#_(spec-key-to-parsed-args :strictly-specking.test-schema/compiler)

#_(find-key-path-without-ns :fig-opt/real :car1)


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
