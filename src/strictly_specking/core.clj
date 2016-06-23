(ns strictly-specking.core
  (:require
   [strictly-specking.parse-spec]
   [strictly-specking.strict-keys :as strict-impl]
   [clojure.pprint :as pp]
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.spec :as s]))

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
                                   :fig-opt/house]))
    
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
    (s/def :fig-opt/builds           (s/+ :fig-opt/build-config))
    
    
    (s/def :project-top/figwheel (strict-keys
                                  :opt-un [:fig-opt/http-server-root
                                           :fig-opt/server-port
                                           :fig-opt/server-ip]
                                  :req-un [:fig-opt/builds]))
    
    )
  )


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








#_ (def terrors (s/explain-data :fig-opt/builds [{:compiler 1
                                                   :id 1
                                                    ;:source-paths 2
                                                  }]))

;; moving from errors to messages

;; filtering errors
;;  upgrading unknown key errors to misplaced key errors or misplaced misspelled key errors
;;  could remove the possible missing required key errors caused by misspelling
;;  could remove the local versions of these "duplicate" errors in strict-keys
;;  perhaps add a reconizable type


(defn missing-keys [error]
  (when (sequential? (:pred error))
    (->> (:pred error)
     (map (fn [[cont _ id]]
            (and (= cont 'contains?)
                 (keyword? id)
                 id)))
     (filter identity)
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


#_(filter-errors (::s/problems terrors))


;; sorting errors
;;  give types to errors
;;  sort them by importance

(def error-precedence
  [::misspelled-key
   ::wrong-key
   ::missing-required-keys
   ::unknown-key
   ::should-not-be-empty
   ::bad-value])

(defn sort-errors [errors]
  (sort-by
   (fn [v] (let [pos (.indexOf error-precedence (::error-type v))]
             (if (neg? pos) 10000 pos))) errors))

;; error-message function
;;   this will provide a more meaningful description of whats gone wrong
;;   will delegete to a ns/key based error message if available

;; temporary copy from spec until public fns make it into api
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
      (when-not (#{:pred :val :reason :via :in} k)
        (print "\n\t" k " ")
        (pr v)))
    (newline)))

#_(->> terrors
      ::s/problems
      filter-errors
      sort-errors
      #_(map message-default-str))

;; TODO fill this out
(def symbol-type-table
  {'string? 'String
   'keyword? 'Keyword })

(defn handle-symbol-pred [sym]
  (if-let [typ (symbol-type-table sym)]
    (str "It should probably be a " (str typ))
    (str "It should satisfy " (str sym))))

(defn format-predicate-str [{:keys [pred] :as e}]
  (cond
    (symbol? pred) (handle-symbol-pred pred)
    :else (str "It should satisfy "
               (pr-str pred))))

(defmulti error-message ::error-type)

(defmethod error-message :default [e]
  (message-default-str e))

(defmethod error-message ::bad-value [{:keys [path pred val reason via in] :as e}]
  (str "The key "
       (pr-str (last in))
       " has the wrong value. "
       (format-predicate-str pred)))

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

