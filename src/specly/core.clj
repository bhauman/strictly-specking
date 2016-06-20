(ns specly.core
  (:require
   [clojure.set :as set]
   [clojure.spec :as s]))



#_(s/explain-data :fig-opt/builds [{:compiler 1
                                    :id 1
                                    :source-paths 2}])
;; similar key implementation
;; this can be improved based on the size of the word

(defn- next-row
  [previous current other-seq]
  (reduce
    (fn [row [diagonal above other]]
      (let [update-val (if (= other current)
                          diagonal
                          (inc (min diagonal above (peek row))))]
        (conj row update-val)))
    [(inc (first previous))]
    (map vector previous (next previous) other-seq)))

(defn levenshtein
  "Compute the levenshtein distance between two [sequences]."
  [sequence1 sequence2]
  (peek
    (reduce (fn [previous current] (next-row previous current sequence2))
            (map #(identity %2) (cons nil sequence2) (range))
            sequence1)))

(defn similar-key* [thresh ky ky2]
  (and (keyword? ky)
       (keyword? ky2)
       #_(= (namespace ky) (namespace ky2))
       (let [dist (levenshtein (str ky) (str ky2))]
         (when (<= dist thresh)
           dist))))

(def similar-key (partial similar-key* 3))

(defn spelling-suggestions [kys ky]
  (->> kys
       (map (juxt identity #(similar-key ky %)))
       (filter second)))


(defprotocol FuzzySpec
  (fuzzy-conform-score [_ x]))

(defn spec-from-registry [spec-key]
  (get (s/registry) spec-key))

(defn fuzzy-spec? [spec-key]
  (when-let [s (and spec-key (spec-from-registry spec-key))]
       (satisfies? FuzzySpec s)))

(declare parse-keys-args poss-path)

;; score a spelling suggestion
;;  + 1 the value is a collection and conforms
;;  + 1 the value is complex and fuzzy conforms <-- TODO
;;  + 1 the value conforms is a bonus

;; TODO is a letter difference equivalient to the above factors?
;; probably not maybe, it should be a second order sorting factor
;; I'm dividing it by 10 to let the above factors determine best fit

;; to do this right this should be done with probabilities
(defn score-suggestion [k->s map-x ky [suggested-key score]]
  (let [key-val  (get map-x ky)
        key-spec (k->s suggested-key)
        valid    (s/valid? key-spec key-val)]
    [suggested-key
     (cond-> (/ score 10)
       ;; TODO: I don't think this is neccessary at all
       (and valid (coll? key-val)) dec
       valid (- 1/10) ;; a single letter difference for being an atomic value and correct
       (and (not valid) (map? key-val) (fuzzy-spec? key-spec))
       (- (or (fuzzy-conform-score (spec-from-registry key-spec) key-val)
              0)))]))

(defn spelling-suggestion [{:keys [k->s known-keys]} map-x ky]
  (->> (spelling-suggestions known-keys ky)
       ;; if this is alrea
       (filter (fn [[sug _]] (not (contains? map-x sug))))
       (map (partial score-suggestion k->s map-x ky))
       (sort-by second)
       ffirst))

;; suggest a completely different key if its a complex value and it conforms to
;; another key that isn't already in the map

(comment

  (s/valid? :fig-opt/build-config {:id "asdf" :source-paths ["as"]})
  
  (score-suggestion
   {:build-config :fig-opt/build-config}
   {:build-confi {:id "asdf" :source-paths 1 #_["as"]}}
   :build-confi [:build-config 1] )
  
  ;; if there are no suggestions we can look at keys that this complex value conforms to
  
  (similar-key :asdff/asdf :asdf/asd)
  
  (spelling-suggestions   #{:asdf/asdf :asdf/abg :asdfffff/a}  :asdf/asd )
  )


;; suggest a completely different key if its a complex value and it conforms to
;; another key that isn't already in the map

;; when a key val is unknown we can rank it against all the keys that are not take in the map

(defn replacement-suggestions [{:keys [k->s known-keys]} map-x ky]
  (when-let [unused-keys (not-empty (filter (complement map-x) known-keys))]
    (let [key-val (get map-x ky)]
      (not-empty
       (keep
        (fn [k]
          (let [unused-spec (k->s k)]
            (cond
              (and (coll? key-val)
                   (s/valid? unused-spec key-val))
              [k 1]
              (map? key-val)
              (cond
                (s/valid? unused-spec key-val)
                [k 1]
                (fuzzy-spec? unused-spec)
                (let [score (fuzzy-conform-score (spec-from-registry unused-spec) key-val)]
                  (when (> score 6/10)
                    [k score]))))))
        unused-keys)))))

(defn replacement-suggestion [key-spec-data map-x ky]
  (->> (replacement-suggestions key-spec-data map-x ky)
       (sort-by (comp - second))
       ffirst))



(comment
  (s/explain :fig-opt/build-config {:id "asdf", :source-paths [""], :assert 1})
  
  (replacement-suggestion (parse-keys-args
                            :opt-un [:fig-opt/http-server-root
                                     :fig-opt/server-port
                                     :fig-opt/server-ip]
                            :req-un [:fig-opt/build-config])
                           {;:http-server-root 1
                            :server-por 1
                                        ;:asdf 1
                                        ;:badly 3
                            :asdf {:id "asdf"
                                   :source-paths [""]
                                   :assert true
                                   ;:madly 2
                                   
                                   }}
                           :asdf
                           )

  )



;; thoughts about fuzzy conform
;; the reason for fuzzy conform is two fold
;; - to determine the key/type of a map (to eventually see if a key belongs somewhere else)
;; - to have a more nuanced way of determining a unknown key replacement suggestion
;;
;; fuzzy conform is about data shape
;; it determines if the keys in a map are in the keyset
;; and recurses on map values to see if they fuzzy conform as well
;; it tries different misspellings for complex valued keys until it gets
;; a successful fuzzy conform

;; I could trigger a fuzzy conform with a binding *fuzzy*
;; and cause conform to behave differently 
;; - this would cause atomic value keys to be validated
;; I could walk the structure myself

;; what I'm really wanting is a measurement of closeness to a type
;; we can use the following data to compute this

;; count of correct keys and correctable keys
;; count of unknown keys
;; count of conforming leaf values
;; count of bad leaf values

;; we can memoize this calculation
;; we could have a *fuzzy-depth-limit*
;; we will add a method to our mapkeys implementation
;;   protocol FuzzySpec
;;     fuzzy-conform-score
;; in that method we will only conform atomic values
;; 
;; and only recurse on instances of fuzzy-spec
;; 


;; digressions -----
;; (optionally we can have a "conform-score" method without the fuzzy part that perhaps considers
;; the role of required and unrequired keys)  
;; -----

#_(merge-with + {} {:asdf 1} {:fda 1} {:fda 1})

;; ingnore nil average
(defn- avg [args]
  (when-let [args (not-empty (keep identity args))]
    (/ (apply + args)
       (count args))))

;; TODO clean this up
;; integrate into :replace-key functionality
;; and spelling scoring
;; think about this and the shared functionality

;; this should be memoized on a per call basis if specs are changing globally
(defn fuzzy-conform-score-helper [{:keys [known-keys  k->s] :as spec-key-data} map-x]
  (when (map? map-x)
    (let [key-to-score
          (fn [[k v]]
            (cond
              (map? v)
              ;; could also check non fuzzy maps pretty easily right here if
              ;; they are described as (s/keys )
              (cond
                (s/valid? (k->s k) v) 1
                (fuzzy-spec? (k->s k))
                (fuzzy-conform-score (spec-from-registry (k->s k)) v)
                
                ;; TODO extract as method
                ;; consider making conform-score protocol private and having a general conform score
                ;; to test for shape of either keys or strict-keys type

                ;; this may be going to deep
                :else
                (when-let [s (spec-from-registry (k->s k))]
                    (when (s/spec? s)
                      (let  [d (s/describe s)]
                        (when (#{'keys 'strict-keys} (first d))
                          (fuzzy-conform-score-helper (apply parse-keys-args (rest d))
                                                    v))))))
              
              (and (sequential? v)
                   (s/valid? (k->s k) v))
              1
              
              ;; if a sequence of maps? set a binding and recur??? good idea???
              ;;   could put score result as meta data on conform result
              :else nil))
          good-keys (filter known-keys (keys map-x))
          unk-keys  (filter (complement known-keys) (keys map-x))
          adjusted-keys
          (->> unk-keys
               (mapv #(vector % (spelling-suggestion spec-key-data map-x %)))
               (filter second))
          good-key-val-scores
          (->> (select-keys map-x good-keys)
               (keep key-to-score))
          adjusted-key-val-scores (->> (map (fn [[oldk newk]] [newk (get map-x oldk)]) adjusted-keys)
                                       (keep key-to-score))
          good-key-count (+ (count good-keys)
                            (count adjusted-keys))
          good-key-score (when-not (zero? (count map-x))
                           (/ good-key-count
                              (count map-x)))]
      #_(prn (into (vec good-key-val-scores) adjusted-key-val-scores))
      (avg [good-key-score
            (avg (into (vec good-key-val-scores) adjusted-key-val-scores))]))))





(comment


  
  (fuzzy-conform-score (strict-keys :opt-un [:fig-opt/http-server-root
                                             :fig-opt/server-port
                                             :fig-opt/server-ip]
                                    :req-un [:fig-opt/loose-build-config])
                       {:http-server-root 1
                        :server-port 1
                        :asdf 3
                        :loose-build-config {;:aaaaaaaaa "asdf"
                                             ;:bbbbbbbb "asdf"
                                             :cccccccc "asdf"
                                             }
                        }
                       )
  
  
  (fuzzy-conform-score-helper (parse-keys-args
                               :opt-un [:fig-opt/http-server-root
                                        :fig-opt/server-port
                                        :fig-opt/server-ip]
                               :req-un [:fig-opt/build-config])
                              {;:http-server-root 1
                               :server-por 1
                               ;:asdf 1
                               ;:badly 3
                               :build-confi {:id "asdf"
                                             :source-paths [""]
                                             :assert 1
                                             ;:madly 2

                                              }}
                              )

  )



;; strict-keys

(defn strict-mapkeys-impl [{:keys [known-keys k->s] :as spec-key-data} keys-spec]
  {:pre [(set? known-keys)]}
  (let [strict (s/spec-impl known-keys #(every? known-keys (keys %))
                            nil nil)]
    (reify
      clojure.lang.IFn
      (invoke [this x] (and (keys-spec x) (strict x)))
      FuzzySpec
      (fuzzy-conform-score [_ x]
        (fuzzy-conform-score-helper spec-key-data x))
      clojure.spec/Spec
      (conform* [_ x]
        (let [result (s/conform* keys-spec x)]
          (if (and (not= :clojure.spec/invalid result)
                   (s/valid? strict x))
            result
            :clojure.spec/invalid)))
      (unform* [_ x] (s/unform* keys-spec x))
      (explain* [_ path via in x]
        (not-empty
         (merge
          (s/explain* keys-spec path via in x)
          (when (map? x)
            (let [[pred-path exp-data]
                  (first (s/explain* strict path via in x))]
              (into {}
                    (->> (keys (:val exp-data))
                         (filter (complement known-keys))
                         (map
                          (fn [unknown-key]
                            (if-let [suggest (spelling-suggestion spec-key-data x unknown-key)]
                              [(conj pred-path :misspelled-key unknown-key)
                               (-> exp-data
                                 (assoc 
                                  ::misspelled-key unknown-key
                                  ::correct-key suggest))]
                              (if-let [replace-suggest (replacement-suggestion spec-key-data x unknown-key)]
                                [(conj pred-path :wrong-key unknown-key)
                                 (-> exp-data
                                     (assoc 
                                      ::wrong-key unknown-key
                                      ::correct-key replace-suggest))]
                                [(conj pred-path :unknown-key unknown-key)
                                 (-> exp-data
                                     (assoc ::unknown-key unknown-key))])))))))
            ))))
      ;; These can be improved
      (gen* [_ a b c]
        (s/gen* keys-spec a b c))
      (with-gen* [_ gfn]
        (s/with-gen* keys-spec gfn))
      (describe* [_] (cons 'strict-keys (rest (s/describe* keys-spec))))
      
      )
    ))

;; hate to duplicate this logic, but it runs at compile time, so, theres that
(defn parse-keys-args [& {:keys [req opt req-un opt-un]}]
  (let [spec-specs  (into (vec req) opt)
        un-specs    (into (vec req-un) opt-un)
        known-keys  (into spec-specs (mapv #(-> % name keyword) un-specs))
        keys->specs (zipmap known-keys (into spec-specs un-specs))]
    {:keys->specs keys->specs
     :k->s        #(or (keys->specs %) %)
     :known-keys  (set known-keys)})) 

#_(apply parse-keys-args [:opt-un [:fig-opt/http-server-root
                           :fig-opt/server-port
                             :fig-opt/server-ip]
                    :req-un [:fig-opt/builds]
                    :req     [:fig-opt/builders]
                    :opt     [:fig-opt/housers]])


(defmacro strict-keys
  "This is a spec that has the same signature as the clojure.spec/keys spec.
  The main difference is that it fails on keys that are not the 
  [:req :opt :req-un :opt-un] specifications.

  This spec will provide an explanation for each unknown key."
  [& args]
  ;; check the args with s/keys
  (let [form (macroexpand `(s/keys ~@args))]
    `(strict-mapkeys-impl (specly.core/parse-keys-args ~@args) ~form)))



(comment
  (do
    (s/def :fig-opt/housers1 string?)
    (s/def :fig-opt/housers2 string?)
    (s/def :fig-opt/housers3 string?)
    
    (s/def :fig-opt/house (strict-keys
                           :opt-un [:fig-opt/housers1 
                                    :fig-opt/housers2 
                                    :fig-opt/housers3
                                    :fig-opt/car1]))
    
    (s/def :fig-opt/car1 string?)
    (s/def :fig-opt/car2 string?)
    (s/def :fig-opt/car3 string?)
    
    (s/def :fig-opt/car (strict-keys
                         :opt-un [:fig-opt/car1
                                  :fig-opt/car2
                                  :fig-opt/car3]))
    
    (s/def :fig-opt/real (strict-keys
                          :opt-un [:fig-opt/car
                                   :fig-opt/house])

      )
    
    
    )
  )



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

(def int-key {:ky ::int-key})

(def int-key? #(= ::int-key (:ky %)))

(defn regex-poss-path [f desc]
  (let [path-set (poss-path f desc)]
    (if (is-regex? desc)
      ;; remove ::int-key from beginning of the paths
      (path-set-fmap #(if (int-key? (first %)) (rest %) %)
                     path-set)
      path-set)))

(defn expanded-map-of-desc? [desc]
  (let [[and' [coll-of' [tuple' key-pred val-pred] empty-map'] map?'] desc]
    (and (= and' 'and)
         (= coll-of' 'coll-of)
         (= tuple' 'tuple)
         (= empty-map' {})
         (= map?' 'map?))
    [key-pred val-pred]))

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
          key-paths       (partial key-paths f)]
      (condp = (and (sequential? desc)
                    (first desc))
        'keys        (key-paths desc)
        'strict-keys (key-paths desc)
        'or          (path-set-join (map poss-path
                                         (desc-keyed-vals desc)))
        ;; this is really the intersection of paths, but there is a notion of
        ;; a infinite path set so ...
        'and         (or (handle-expanded-map-of desc)
                         (path-set-join (map poss-path (desc-vals desc))))
        
        '*     (path-set-cons int-key (regex-poss-path (second desc)))
        '+     (path-set-cons int-key (regex-poss-path (second desc)))
        '?     (path-set-cons int-key (regex-poss-path (second desc)))
        'alt   (path-set-cons int-key
                              (path-set-join (map regex-poss-path (desc-keyed-vals desc))))
        'cat   (path-set-cons int-key
                              (path-set-join (map regex-poss-path (desc-keyed-vals desc))))
        '&     (path-set-cons int-key (regex-poss-path (second desc)))
        ;; cat is a splicing operation something without a numeric path is going to get one consed on
        
        
        'tuple   (path-set-cons int-key
                                (path-set-join (map poss-path (desc-vals desc))))
        'col-of       (path-set-cons int-key (poss-path (second desc)))
        'col-checker  (path-set-cons int-key (poss-path (second desc)))
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



;; moving from errors to messages

;; filtering errors
;; upgrading unknown key errors to misplaced key errors or misplaced misspelled key errors
;; could remove the possible missing required key errors caused by misspelling
;; could remove the local versions of these errors in strict-keys




;; filtering and sorting errors
;; 








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



(comment

  (s/describe :fig-opt/builds)
  

  (do
    (s/def :build-config/id (s/or :string string? :keyword keyword?))
    (s/def :build-config/source-paths (s/+ string?))
    (s/def :build-config/assert #(or (true? %) (false? %)))
    (s/def :fig-opt/build-config (strict-keys :req-un [
                                                       :build-config/id
                                                       :build-config/source-paths]
                                              :opt-un [:build-config/assert]))

    (s/def :fig-opt/loose-build-config (s/keys :req-un [
                                                        :build-config/id
                                                        :build-config/source-paths]
                                               :opt-un [:build-config/assert]))
    
    
    (s/def :fig-opt/http-server-root string?)
    (s/def :fig-opt/server-port      integer?)
    (s/def :fig-opt/server-ip        string?)
    (s/def :fig-opt/builds           (s/* :fig-opt/build-config))
    
    
    (s/def :project-top/figwheel (strict-keys
                                  :opt-un [:fig-opt/http-server-root
                                           :fig-opt/server-port
                                           :fig-opt/server-ip]
                                  :req-un [:fig-opt/builds]))
    

    )

  )







(comment
  
  (s/explain-str :project-top/figwheel {; :hey 1
                                        ;:there 2
                                      :server-por "heyy"
                                      ;:server-port "heyy"
                                      :asdff
                                      [{:id :asdf
                                        :source-paths ["src"]}]})
  

  (remove-ns 'specly.core)
  )
