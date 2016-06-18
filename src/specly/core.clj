(ns specly.core
  (:require
   [clojure.set :as set]
   [clojure.spec :as s]))



#_(s/explain-data :fig-opt/builds [{:compiler 1
                                    :id 1
                                    :source-paths 2}])
;; similar key implementation

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
       valid dec
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

(s/explain :fig-opt/build-config {:id "asdf", :source-paths [""], :assert 1})

(comment

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
                (fuzzy-conform-score (spec-from-registry (k->s k)) v))

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
  (let [strict (s/spec-impl known-keys #(every? known-keys (keys %)) nil nil)]
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
                                 #_(update-in [:in] conj unknown-key)
                                 (assoc 
                                  ::misspelled-key unknown-key
                                  ::correct-key suggest))]
                            (if-let [replace-suggest (replacement-suggestion spec-key-data x unknown-key)]
                              [(conj pred-path :misspelled-key unknown-key)
                               (-> exp-data
                                   #_(update-in [:in] conj unknown-key)
                                   (assoc 
                                    ::misspelled-key unknown-key
                                    ::correct-key suggest))]
                              [(conj pred-path :unknown-key unknown-key)
                               (-> exp-data
                                   #_(update-in [:in] conj unknown-key)
                                   (assoc ::unknown-key unknown-key))])
                            
                            )))))))))
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
    `(strict-mapkeys-impl ~(apply parse-keys-args args) ~form)))



(s/def :build-config/id (s/or :string string? :keyword keyword?))
(s/def :build-config/source-paths (s/+ string?))
(s/def :build-config/assert #(or (true? %) (false? %)))
(s/def :fig-opt/build-config (strict-keys :req-un [
                                                   :build-config/id
                                                   :build-config/source-paths]
                                          :opt-un [:build-config/assert]))


(s/def :fig-opt/http-server-root string?)
(s/def :fig-opt/server-port      integer?)
(s/def :fig-opt/server-ip        string?)
(s/def :fig-opt/builds           (s/+ :fig-opt/build-config))


(s/def :project-top/figwheel (strict-keys
                              :opt-un [:fig-opt/http-server-root
                                       :fig-opt/server-port
                                       :fig-opt/server-ip]
                              :req-un [:fig-opt/builds]))

(comment
  
  (s/explain-str :project-top/figwheel {:hey 1
                                      :there 2
                                      :serve-port 1 #_"heyy"
                                      ;:server-port "heyy"
                                      :builds
                                      [{:id :asdf
                                        :source-paths ["src"]}]})
  
  )





#_(with-meta :hey {:d 1})


#_(s/def :builds  (s/cat :builds (s/+ ))  )
