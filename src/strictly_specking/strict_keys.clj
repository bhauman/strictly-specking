(ns strictly-specking.strict-keys
  (:require
   [clojure.set :as set]
   [clojure.spec :as s]
   [clojure.string :as string]
   [clojure.pprint :as pp]
   [strictly-specking.fuzzy :refer [similar-key]]
   [strictly-specking.ansi-util :refer [color-text]]
   [strictly-specking.annotated-pprint :refer [pprint-notes annotate-path]]
   [strictly-specking.parse-spec :as parse :refer [parse-keys-args spec-from-registry]]))

(def
  ^{:dynamic true
    :doc
    "The *strict-level* can be bound to either 
    :ignore - ignore any unknown key problems
    :warn   - warn on unknown keys 
  or the default
    :fail   - have the spec fail with an unknown-key explanation (default)" }
  *unknown-key-level*
  :fail)

;; similar key implementation
;; this can be improved based on the size of the word
;; TODO improve this by looking at the implementation in type_check.clj
;; TODO perhaps steal the algorithm in clojurescript

;; TODO fuzzy-conform-helper may not need to be a part of the strict
;; keys spec object and it could be made stronger by using the new
;; spec/describe traversing code (ie we can get all the keys for any
;; given spec now)

;; TODO memoization of fuzzy-conform-helper on a per call basis

(defn spelling-suggestions [kys ky]
  (->> kys
       (map (juxt identity #(similar-key ky %)))
       (filter second)))

(defprotocol FuzzySpec
  (fuzzy-conform-score [_ x]))

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
        valid    (or (and (spec-from-registry key-spec)
                          (s/valid? key-spec key-val))
                     true)]
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

  (s/explain-data :fig-opt/build-config {:id "asdf" :source-paths ["as"]})
  
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
              ;; making this be more complex
              (and (and (coll? key-val) (coll? (first key-val))) 
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
(defn fuzzy-conform-score-helper [{:keys [known-keys k->s] :as spec-key-data} map-x]
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

(defn fuzzy-conform-score-spec-key [spec-key map-x]
  {:pre [(map? map-x) (spec-from-registry spec-key)]}
  (fuzzy-conform-score-helper
   (parse/spec-key-to-parsed-args spec-key)
   map-x))

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

(defmulti conform-at-level (fn [& args] @#'*unknown-key-level*))

(defmethod conform-at-level :default [_ x strict keys-spec known-keys]
  (let [result (s/conform* keys-spec x)]
    (if (and (not= ::s/invalid result)
             (s/valid? strict x))
      result
      ::s/invalid)))

(defmethod conform-at-level :warn [_ x strict keys-spec known-keys]
  (let [result (s/conform* keys-spec x)]
    (when (and
           (map? x)
           (not (s/valid? strict x)))
      (when-let [unknown-keys (not-empty (filter (complement known-keys) (keys x)))]
        (println (str
                  (color-text "Strict Keys Spec Warning: Unknown Keys " :yellow)
                  (color-text (pr-str (vec unknown-keys)) :red)
                  (color-text " found in map:" :yellow)))
        (pprint-notes
         (annotate-path x []
                        {:comments (zipmap unknown-keys
                                           (repeat
                                            {:comment "unknown key"
                                             :key-colors     [:red]
                                             :value-colors   [:none]
                                             :comment-colors [:magenta]}))}))
        #_(pp/pprint x)
        (println (color-text "The only expected keys are:" :yellow))
        (pp/pprint known-keys)))
    result))

(defmethod conform-at-level :ignore [_ x strict keys-spec known-keys]
  (s/conform* keys-spec x))

;; strict-keys

(defn strict-mapkeys-impl [{:keys [known-keys k->s keys->specs] :as spec-key-data} keys-spec]
  {:pre [(set? known-keys)]}
  (let [strict (s/spec-impl (list 'every? known-keys '(keys %))
                            #(every? known-keys (keys %))
                            nil nil)]
    (reify
      clojure.lang.IFn
      (invoke [this x] (and (keys-spec x) (strict x)))
      FuzzySpec
      (fuzzy-conform-score [_ x]
        (fuzzy-conform-score-helper spec-key-data x))
      s/Spec
      (conform* [self x]
        (conform-at-level self x strict keys-spec known-keys))
      (unform* [_ x] (s/unform* keys-spec x))
      (explain* [_ path via in x]
        (not-empty
         (vec
          (concat
           (s/explain* keys-spec path via in x)
           (when (and (map? x) (not (#{:warn :ignore} *unknown-key-level*)))
             (when-let [exp-data (first (s/explain* strict path via in x))]
               (when (map? (:val exp-data))
                 (->> (keys (:val exp-data))
                      (filter (complement known-keys))
                      (mapv
                       (fn [unknown-key]
                         (assoc exp-data
                                :path (conj (:path exp-data) :unknown-key unknown-key)
                                :strictly-specking.core/keys->specs keys->specs
                                :strictly-specking.core/unknown-key unknown-key)))))))))))
      ;; These can be improved
      (gen* [_ a b c]
        (s/gen* keys-spec a b c))
      (with-gen* [_ gfn]
        (s/with-gen* keys-spec gfn))
      (describe* [_] (cons 'strict-keys (rest (s/describe* keys-spec)))))))
