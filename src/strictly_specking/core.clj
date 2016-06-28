(ns strictly-specking.core
  (:require
   [strictly-specking.parse-spec]
   [strictly-specking.strict-keys :as strict-impl]
   [strictly-specking.ansi-util :as cl :refer [color]]
   [strictly-specking.annotated-pprint :as annot]
   [strictly-specking.edn-string-nav :as edn-string-nav]
   [strictly-specking.context-print :as cp]
   [clansi.core :refer [with-ansi]]
   [clojure.pprint :as pp]
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.spec :as s]))

;; * some initial rules to test with
;; 



;; * strict-keys macro

(defmacro strict-keys
  "This is a spec that has the same signature as the clojure.spec/keys spec.
  The main difference is that it fails on keys that are not the 
  [:req :opt :req-un :opt-un] specifications.

  This spec will provide an explanation for each unknown key."
  [& args]
  ;; check the args with s/keys
  (let [form (macroexpand `(s/keys ~@args))]
    `(strictly-specking.strict-keys/strict-mapkeys-impl
      (strictly-specking.parse-spec/parse-keys-args ~@args) ~form)))

;; * moving from errors to messages

;; ** filtering errors
;;
;; *** upgrading unknown key errors to misplaced or misplaced misspelled key errors
;; *** could remove the possible missing required
;; *** key errors caused by misspelling could remove the local versions of
;;     these "duplicate" errors in strict-keys perhaps add a reconizable type
;; *** TODO need to add the original structure that the errors are on

#_ (def terrors (s/explain-data :fig-opt/builds [{:compiler 1
                                                  :id 1
                                                  :source-path 2
                                                  }]))

;; *** detect missing-keys 

(defn missing-keys [error]
  (when (and (sequential? (:pred error))
             (every? (every-pred sequential?
                                 #(= (first %) 'contains?)
                                 #(keyword? (last %)))
                     (:pred error)))
    (->> (:pred error)
         (map last)
         not-empty)))

#_(missing-keys {:pred '[(contains? % :id) (contains? % :source-paths)],
                 :val {:compiler 1},
                 :via [:fig-opt/builds :fig-opt/builds :fig-opt/build-config :fig-opt/build-config],
                 :in [0]})

;; *** mark errors with a type


(defn error-type [error]
  (cond
    (error ::unknown-key)      ::unknown-key
    (error ::misspelled-key)   ::misspelled-key
    (error ::wrong-key)        ::wrong-key
    (error ::missing-keys)     ::missing-required-keys
    (and (= (:reason error) "Insufficient input")
         (= (:val error) '())) ::should-not-be-empty
    :else ::bad-value))

(defn add-error-type [error]
  (assoc error ::error-type (error-type error)))

;; *** parse out and added missing keys data on error

(defn add-required-keys [error]
  (if-let [missing-required-keys (missing-keys error)]
    (assoc error ::missing-keys (missing-keys error))
    error))

;; *** apply initial data transforms on errors 

(defn filter-errors [problems]
  (->> problems
       (map (fn [[k v]] (assoc v :path k)))
       (map add-required-keys)
       (map add-error-type)))


;; *** combine errors for single location
;; Sometimes there are multiple errors on the same path location

(defn combined-or-pred [errors]
  (let [errors-to-comb
        (->> errors
             (filter #(= (::error-type %) ::bad-value))
             (group-by :in )
             (filter #(< 1 (count (second %))))
             (map (fn [[_ errors]]
                    (assoc (first errors)
                           :pred (cons 'or (map :pred errors))
                           :path (cons ::combined-path (map :path errors))
                           ::error-type ::bad-value-comb-pred))))
        paths (set (map :in errors-to-comb))]
    (concat   (filter #(not (paths (:in %))) errors)
              errors-to-comb)))

;; *** corrections overide missing
;; if there is an error with a ::correct-key in it eliminate any ::missing-required-keys reference

(defn l [t x]
  (prn t x)
  x)

(defn corrections-overide-missing-required [errors]
  (if-let [corrected-key-errors (not-empty (filter ::correct-key errors))]
    (let [key-paths  (set (map (juxt ::correct-key :in)
                               corrected-key-errors))
          fixed-errs
          (->> errors
               (keep (fn [e]
                       (if-let [kps (and (::missing-keys e)
                                         (not-empty
                                          (set/intersection
                                           key-paths
                                           (set (map #(vector % (:in e)) (::missing-keys e))))))]
                         (when-let [keys-left  (not-empty (set/difference (set (::missing-keys e))
                                                                          (set (map first key-paths))))]
                                       (assoc e ::missing-keys (seq keys-left)))
                         e)))
               not-empty)]
      fixed-errs)
    errors))



#_(->> terrors
      ::s/problems
      filter-errors
      sort-errors
      combined-or-pred
      corrections-overide-missing-required
      #_(map error-message)
      #_(map println))

#_(filter-errors (::s/problems terrors))


;; ** sorting errors
;;  give types to errors
;;  sort them by importance

(def error-precedence
  [::misspelled-key
   ::wrong-key
   ::missing-required-keys
   ::unknown-key
   ::should-not-be-empty
   ::bad-value-comb-pred
   ::bad-value])

(defn sort-errors [errors]
  (sort-by
   (fn [v] (let [pos (.indexOf error-precedence (::error-type v))]
             (if (neg? pos) 10000 pos))) errors))


;; still rough
;; need a notion of a source ;; pure data vs. file source
(defn prepare-errors [explain-data validated-data file]
  (->> explain-data
       ::s/problems
       filter-errors
       (map #(assoc % ::root-data validated-data))
       (map #(if file
                 (assoc % ::file-source file)
               %))       
       sort-errors
       combined-or-pred
       corrections-overide-missing-required))

;; ** error-message function
;; this will provide a more meaningful description of whats gone wrong
;; will delegete to a ns/key based error message if available

;; *** TODO this needs to be refined to the default error
;; must keep in mind that the error is going to be displayed below
(defn message-default-str
  [{:keys [path pred val reason via in] :as prob}]
  (with-out-str
    (when-not (empty? in)
      (print "In:" in ""))
    (print "val: ")
    (pr val)
    (print " fails")
    (when-not (empty? via)
      (print " spec:" (last via)))
    (when-not (empty? path)
      (print " at:" path))
    (print " predicate: ")
    (pr pred)
    (when reason (print ", " reason))
    (doseq [[k v] prob]
      (when-not (#{:pred :path :val :reason :via :in} k)
        (print "\n\t" k " ")
        (pr v)))
    (newline)))

;; TODO fill this out
(def symbol-type-table
  '{string?  String
    keyword? Keyword
    vector?  Vector
    map?     Map
    integer? Integer
    number?  Number})

(def type-lookup->str (some-fn symbol-type-table pr-str))

(defn resovlable-predicate? [x]
  (or (symbol-type-table x)
   (and
    (sequential? x)
    (= 1 (count x))
    (symbol-type-table (first x)))))

(defn pred-symbol->str [x]
  (if-let [res (resovlable-predicate? x)]
    res
    (type-lookup->str x)))

(defn seq-with-first [x pred]
  (and (sequential? pred) (= x (first pred))))

(defn format-predicate-str [{:keys [pred val]}]
  (cond
    (seq-with-first 'or pred)
    (str (if (every? symbol-type-table (rest pred))
           "It should be one of: "
           "It should satisfy one of: ")
         (string/join " | "
                      (map #(color % :good-pred)
                           (map (some-fn symbol-type-table str) (rest pred)))))
    (seq-with-first '+ pred)
    (str "It should be a non-empty sequence of: " (color
                                                   (pred-symbol->str (rest pred))
                                                   :good-pred))
    (seq-with-first '* pred)
    (str "It should be a sequence of: " (color (pred-symbol->str (rest pred)) :good-pred))
    :else
    (str (if (resovlable-predicate? pred)
           "It should be a "
           "It should satisfy ")
         (color (pred-symbol->str pred) :good-pred))))

(defn format-seq-with-end [s end]
  (let [f (color (pr-str (first s)) :focus-key)]
    (condp = (count s)
      1 f
      2 (str f " " end " "(color (pr-str (second s)) :focus-key))
      (str f ", " (format-seq-with-end (rest s) end)))))

(defn format-seq-with-and [s]
  (format-seq-with-end s 'and))

(defn format-seq-with-or [s]
  (format-seq-with-end s 'or))

(defn format-summarized-value [v]
  (string/trim-newline
   (with-out-str
     (binding [*print-length* 4
               *print-level* 2]
       (pp/pprint v)))))

;; *** TODO figure out how to identify where to point to

;; might not need this yet

(defn parent [e]
  (get-in (::root-data e) (butlast (:in e))))

(defn current [e]
  (get-in (::root-data e) (:in e)))

(defn parent-pred [pred]
  #(pred (parent %)))

(def parent-sequence? (parent-pred sequential?))
(def parent-vector? (parent-pred vector?))
(def parent-map? (parent-pred map?))
(def parent-set? (parent-pred set?))
(def parent-coll? (parent-pred coll?))

(defmulti error-message ::error-type)

(defmethod error-message :default [e]
  (message-default-str e #_(select-keys e [:pred :path :val :reason :via :in])))

(defn parent-type-str [e]
  (cond
    (parent-map? e)      "Map"
    (parent-set? e)      "Set"
    (parent-vector? e)   "Vector"
    (parent-sequence? e) "Sequence"))

(defn bad-value-message [{:keys [path pred val reason via in] :as e}]
  (let [k (last in)]
    (cond
      (parent-map? e)
      (str "  The key " (color (pr-str k) :focus-key)
           " has the wrong value "
           (color (format-summarized-value val) :bad-value)
           ".\n  " (format-predicate-str e))
      (parent-coll? e)
      (str "  The " (parent-type-str e)
           " at " (color (pr-str (vec (butlast in))) :focus-path)
           " contains bad value "
           (color (format-summarized-value val) :bad-value)
           ".\n  " (format-predicate-str e))
      :else
      (str "  The key " (color (pr-str k) :focus-key)
           " has the wrong value "
           (color (format-summarized-value val) :bad-value)
           ".\n  " (format-predicate-str e)))))

(defmethod error-message ::bad-value [e] (bad-value-message e))
(defmethod error-message ::bad-value-comb-pred [e] (bad-value-message e))

(defmethod error-message ::should-not-be-empty [{:keys [path pred val reason via in] :as e}]
  (str "  The value " (color (pr-str val) :bad-value)
       " at key " (color (last in) :focus-key)
       " should not be empty.\n  "
       (format-predicate-str (update-in e [:pred] (fn [x] (list '+ x))))))

(defmethod error-message ::unknown-key [{:keys [path pred val reason via in] :as e}]
  (str "  Found unrecognized key " (color (::unknown-key e) :error-key)
       " at path " (color (pr-str in) :focus-path) "\n"
       "  Must be one of: " (format-seq-with-or pred)))

(defmethod error-message ::missing-required-keys [{:keys [path pred val reason via in] :as e}]
  (when-let [kys (not-empty (::missing-keys e))]
    (if (< 1 (count kys))
      (str "  Missing required keys " (format-seq-with-and kys) " at path " (color (pr-str in) :focus-path))
      (str "  Missing required key "  (format-seq-with-and kys) " at path " (color (pr-str in) :focus-path)))))

;; *** TODO ::wrong-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys
;; but the scores of the top choices should be close and in order 

(defmethod error-message ::wrong-key [{:keys [path pred val reason via in] :as e}]
  (str "  The key " (color (::wrong-key e) :error-key)
       " is unrecognized. Perhaps you meant "
       (color (::correct-key e) :correct-key)
       "?"))

;; *** TODO ::misspelled-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys

(defmethod error-message ::misspelled-key [{:keys [path pred val reason via in] :as e}]
  (str "  The key " (color (::misspelled-key e) :error-key)
       " is misspelled. It should probably be "
       (color (::correct-key e) :correct-key)))

;; *** TODO: fill in the rest of the error types

(defmulti inline-message ::error-type)

(defmethod inline-message :default [e] (str "The key " (-> e :in last) " has a problem"))
(defmethod inline-message ::bad-value [e]
  (str "The key " (-> e :in last) " has a bad value"))

(defmethod inline-message ::bad-value-comb-pred [e]
  (str "The key " (-> e :in last) " has a bad value"))

(defmethod inline-message ::should-not-be-empty [e]
  (str "The key " (-> e :in last) " should not be empty"))

(defmethod inline-message ::unknown-key [e]
  (str "The key " (::unknown-key e) " is unrecognized"))

;; this is interestion I need to add a key
#_(defmethod inline-message ::missing-required-keys [e]
  (str "XXXXX "
       (-> e :in last)
       " is missing"))

(defmethod inline-message ::wrong-key [e]
  (str "The key " (-> e ::wrong-key) " should probably be " (::correct-key e)))

(defmethod inline-message ::misspelled-key [e]
  (str "The key " (-> e ::misspelled-key) " should probably be " (::correct-key e)))


;; *** use pprint to print contextual errors

(defn pprint-sparse-path
  ([data path key-message-map colors]
   (pp/with-pprint-dispatch annot/error-path-dispatch
     (pp/pprint
      (annot/annotate-path-only
       data
       path
       {:abbrev true
        :comments (into {}
                        (map (fn [[k message]]
                               [k
                                (merge {:key-colors     [:error-key]
                                        :value-colors   [:reset]
                                        :comment-colors [:pointer]
                                        :comment message}
                                       colors)])
                             key-message-map))}))))
  ([data path key-message-map]
   (pprint-sparse-path data path key-message-map {})))

;; *** file contextual errors

;; really need to move this towards the same interface as above
;; if there is a file on the error we will display it in context of the file

(defn pprint-in-file [file base-path key-message-map]
  (let [[k message] (first key-message-map) 
        path        (conj (vec base-path) k)]
    (when-let [{:keys [line column value path loc]}
               (edn-string-nav/get-path-in-clj-file path file)]
      (println "  File:" (str file))
      (cp/print-message-in-context-of-file file line column message)
      true)))

#_(edn-string-nav/get-path-in-clj-file [0] "tester.edn")

(defn pprint-in-context
  ([e base-path key-message-map]
   (pprint-in-context e base-path key-message-map {}))
  ([e base-path key-message-map colors]
   (or (and
        (::file-source e)
        (pprint-in-file (::file-source e) base-path key-message-map))
     (pprint-sparse-path (::root-data e) base-path key-message-map colors))))

;; *** pprint inline message
;;
;; this should dispatch and display in file context if there is file
;; information on the error

(defmulti pprint-inline-message ::error-type)

(defmethod pprint-inline-message :default [e]
  (pprint-in-context e (butlast (:in e))
                     {(last (:in e)) (inline-message e)}
                     {:key-colors [:highlight]
                      :value-colors [:bad-value]}))

(defmethod pprint-inline-message ::unknown-key [e]
  (pprint-in-context e (:in e)
                     {(::unknown-key e) (inline-message e)}))

(defmethod pprint-inline-message ::misspelled-key [e]
  (pprint-in-context e (:in e)
                     {(::misspelled-key e) (inline-message e)}))

(defmethod pprint-inline-message ::wrong-key [e]
  (pprint-in-context e (:in e)
                     {(::wrong-key e) (inline-message e)}))

(defn pprint-missing-keys-context [e]
  (pprint-sparse-path (reduce #(assoc-in %1 (conj (vec (:in e)) %2)
                                         '...)
                              (::root-data e)
                              (::missing-keys e))
                      (:in e)
                      (into {}
                            (map (fn [k]
                                   [k (str "The required key " (pr-str k) " is missing")]))
                            (::missing-keys e))))

(defn insert-missing-keys [formatted-lines kys line column]
  (let [length (apply max (map (comp count str) kys))]
    (concat
     (take-while #(= (first %) :line) formatted-lines)
     (let [r (drop-while #(= (first %) :line) formatted-lines)]
       (concat
        [(->> (first r) rest (cons :line) vec)]
        (mapv (fn [k] [:error-line nil
                       (str (cp/blanks column)
                            (format (str "%-" length "s  <- missing required key") k))]) kys)
        (rest r))))))

(defn inline-missing-keys? [e]
  (let [m (get-in (::root-data e) (:in e))]
    (when (<= 2 (count m))
      (let [[first-key second-key] (keys m)
            loc-data1 (edn-string-nav/get-path-in-clj-file (conj (vec (:in e)) first-key)
                                                           (::file-source e))
            loc-data2 (edn-string-nav/get-path-in-clj-file (conj (vec (:in e)) second-key)
                                                           (::file-source e))]
        (when (not= (:line loc-data1) (:line loc-data2))
          loc-data1)))))

(defn pprint-missing-keys-in-file-context [e]
  (let [m (get-in (::root-data e) (:in e))]
    (if-let [{:keys [line column value path loc]}
             (inline-missing-keys? e)]
      (do
        (println "  File:" (str (::file-source e)))
        (-> (cp/fetch-lines (::file-source e))
            (cp/number-lines)
            (cp/insert-message line column
                               (str "Map is missing required key"
                                    (if (= 1 (count (::missing-keys e)))
                                      "" "s")))
            (insert-missing-keys (::missing-keys e) line column )
            (cp/extract-range-from-center line 10)
            cp/trim-blank-lines
            cp/blank-space-trim
            cp/format-line-numbers
            cp/color-lines
            cp/print-formatted-lines)
        true)
      (pprint-in-file (::file-source e)
                      (butlast (:in e))
                      {(last (:in e))
                       (str "Map is missing required key"
                            (if (= 1 (count (::missing-keys e)))
                              (str ": " (pr-str (first (::missing-keys e))))
                              (str "s: " (string/join ", " (map pr-str (::missing-keys e))))
                              ))}))))

(defmethod pprint-inline-message ::missing-required-keys [e]
  (or
   (pprint-missing-keys-in-file-context e)
   (pprint-missing-keys-context e)))

(defn test-print [error]
  (println (color "---------------------------------\n" :header))
  (println (error-message error))
  (println "\n")
  ;; could should indent this
  (pprint-inline-message error)
  (println "\n")
  (println (color "---------------------------------\n" :footer))  
  )

(comment
  (def structer (read-string (slurp "tester.edn")))

  (def terr (s/explain-data :fig-opt/builds structer))

  (with-ansi
    (->> 
     (prepare-errors terr structer "tester.edn")
     #_(map pprint-inline-message)
     (mapv test-print)
     #_ (mapv error-message)
     #_ (map println)))
  )

;; * docs on keys
;;

;; our own private registry for data on keys

(defonce ^:private registry-ref (atom {}))

(defn def-key-doc [k d]
  (swap! registry-ref assoc-in [k ::doc] d))

(defn key-doc [k]
  (get-in @registry-ref [k ::doc]))



;; pprint is in annotated-pprint.clj

;; create an extra error message addendum

;; contextual printing is in print-context.clj

;; create some functionality to add code context to errors if the
;;   source-file(s) is known sjacket can be useful here

;; printing errors
;;   create a intermediate data structure - this will be useful for exception data
;;     :error with the original error
;;     :description a more meaningful description of whats gone wrong with an error message function
;;     :error-on is this on :value or :key
;;     :path to error key
;;     :path-str  path printing -- explore using pprint to get rid of a dependency
;;     :code-context -- error displayed along with code
;;     :alternate-path
;;     :alternate-path-str
;;     :doc   docs
;;  create a printer that outputs formatted error

;; key based error message
;;   hang a method off of an ns/keyword
;;   it takes an error, the complete data structure, a path to the key
;;   and it will provide a better error message than the stock one

;; docs on keys
;;   create a macro that stores docs on namespaced keys
;;   this can should be added to metadata in the registry on strictly-specking/docs etc

;; deeper error explain
;;   hang a method off of a keyword
;;   it takes an error, the complete data structure, a path to the key
;;   and gives a more detailed explaination of why that value is the wrong value
;;   this will hook into the main message error printing

;; explain
;;   create a macro that stores a method on a ns/key
;;   that given all the config data, and a path to the key
;;   that will provide an explanation of the behavior of that key
;;   in its current value, in conjuntion with the rest of the configuration


(defn init-test-rules []
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
                                 :fig-opt/house]))
  
  (s/def :build-config/id (s/or :string string? :keyword keyword?))
  (s/def :build-config/source-paths (s/+ string?))
  (s/def :build-config/asdfasdf (s/* string?))  
  (s/def :build-config/assert #(or (true? %) (false? %)))
  (s/def :fig-opt/build-config (strict-keys :req-un [
                                                     :build-config/id
                                                     :build-config/source-paths]
                                            :opt-un [:build-config/assert
                                                     :build-config/asdfasdf]))

  (s/def :fig-opt/loose-build-config (s/keys :req-un [
                                                      :build-config/id
                                                      :build-config/source-paths]
                                             :opt-un [:build-config/assert
                                                      :build-config/asdfasdf]))
  
  
  (s/def :fig-opt/http-server-root string?)
  (s/def :fig-opt/server-port      integer?)
  (s/def :fig-opt/server-ip        string?)
  (s/def :fig-opt/builds           (s/+ :fig-opt/build-config))
  
  
  (s/def :project-top/figwheel (strict-keys
                                :opt-un [:fig-opt/http-server-root
                                         :fig-opt/server-port
                                         :fig-opt/server-ip]
                                :req-un [:fig-opt/builds]))
  
  
  )

#_ (init-test-rules)



