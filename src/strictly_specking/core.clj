(ns strictly-specking.core
  (:require
   [strictly-specking.parse-spec]
   [strictly-specking.strict-keys :as strict-impl]
   [clojure.pprint :as pp]
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.spec :as s]))

;; * some initial rules to test with
;; 

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

#_ (init-rules)

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

(defn add-required-keys [error]
  (if-let [missing-required-keys (missing-keys error)]
    (assoc error ::missing-keys (missing-keys error))
    error))

(defn filter-errors [problems]
  (->> problems
       (map (fn [[k v]] (assoc v :path k)))
       (map add-required-keys)
       (map add-error-type)))

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

;; ** error-message function
;; this will provide a more meaningful description of whats gone wrong
;; will delegete to a ns/key based error message if available

;; temporary copy from spec until public fns make it into api
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

(defn pred-symbol->str [x]
  (if-let [res (and (sequential? x)
                    (= 1 (count x))
                    (symbol-type-table (first x)))]
    res
    (type-lookup->str x)))

(defn seq-with-first [x pred]
  (and (sequential? pred) (= x (first pred))))

(defn format-predicate-str [{:keys [pred val]}]
  (cond
    (seq-with-first 'or pred)
    (str "It should satisfy one of: "  (string/join " | " (map (some-fn symbol-type-table str) (rest pred))))
    (seq-with-first '+ pred)
    (str "It should be a non-empty sequence of: " (pred-symbol->str (rest pred)))
    (seq-with-first '* pred)
    (str "It should be a sequence of: " (pred-symbol->str (rest pred)))    
    :else
    (str "It should satisfy "          (pred-symbol->str pred))))

(defn format-seq-with-and [s]
  (let [f (pr-str (first s))]
    (condp = (count s)
      1 f
      2 (str f " and " (pr-str (second s)))
      3 (str f ", " (format-seq-with-and (rest s))))))

(defn format-summarized-value [v]
  (string/trim-newline
   (with-out-str
     (binding [*print-length* 4
               *print-level* 2]
       (pp/pprint v)))))

(defmulti error-message ::error-type)

(defmethod error-message :default [e]
  (message-default-str e #_(select-keys e [:pred :path :val :reason :via :in])))

(defn bad-value-message [{:keys [path pred val reason via in] :as e}]
  (let [k (last in)]
    (if
      (and (integer? k)
           (< 1 (count in)))
      ;; *** this is very dicey it might be a map in this case, no way to tell right now
      ;; if we hade the original structure we could test for this
      ;; also we may have pointers into the sequence structure at some point
      ;; and then this will be obviated
      (str "The sequence at key " (second (reverse in)) " has the wrong value "
           (format-summarized-value val)  " at index " (pr-str k)
           ". " (format-predicate-str e))
      (str "The key " (pr-str k) " has the wrong value "
           (format-summarized-value val) ". " (format-predicate-str e))))
#_  (pp/pprint e)
)

(defmethod error-message ::bad-value [e] (bad-value-message e))
(defmethod error-message ::bad-value-comb-pred [e] (bad-value-message e))

(defmethod error-message ::should-not-be-empty [{:keys [path pred val reason via in] :as e}]
  (str "The value " (pr-str val) " at key " (last in) " should not be empty. "
       (format-predicate-str (update-in e [:pred] (fn [x] (list '+ x))))))

(defmethod error-message ::unknown-key [{:keys [path pred val reason via in] :as e}]
  (str "Found unrecognized key " (::unknown-key e) " at path " (pr-str in)))

(defmethod error-message ::missing-required-keys [{:keys [path pred val reason via in] :as e}]
  (pp/pprint e)
  (when-let [kys (not-empty (::missing-keys e))]
    (if (< 1 (count kys))
      (str "Is missing required keys " (format-seq-with-and kys) " at path " (pr-str in))
      (str "Is missing required key " (format-seq-with-and kys) " at path " (pr-str in)))))

;; *** TODO ::wrong-key

;; *** TODO ::misspelled-key

#_ (def terr (s/explain-data :fig-opt/builds [{:compiler 1
                                               :id 1
                                               :asdfasdf [1]
                                               :cowabunga ["asdf"]
                                               }]))

#_(->> terr
      ::s/problems
      filter-errors
      combined-or-pred
      corrections-overide-missing-required
      sort-errors
      (mapv error-message)
      (map println))





;; TODO: fill in the rest of the error types

;; start on using pprint to print contextual errors

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

