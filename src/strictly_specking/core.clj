(ns strictly-specking.core
  (:require
   [clansi.core :refer [with-ansi without-ansi]]
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.spec :as s]
   [clojure.string :as string]
   [strictly-specking.annotated-pprint :as annot]
   [strictly-specking.ansi-util :as cl :refer [color]]
   [strictly-specking.context-print :as cp]
   [strictly-specking.edn-string-nav :as edn-string-nav]
   [strictly-specking.error-printing :refer [error-message
                                             inline-message
                                             pprint-inline-message] :as ep]
   [strictly-specking.fix-paths :as fix-paths]
   [strictly-specking.fuzzy :refer [similar-key]]
   [strictly-specking.parse-spec :as parse]
   [strictly-specking.path-matching :as path-match]
   [strictly-specking.strict-keys :as strict-impl]))

#_(remove-ns 'strictly-specking.core)

;; * TODO move to predicates load file

(defn non-blank-string? [x]
  (and (string? x) (not (string/blank? x))))

;; * A def-key construct

;; Creating a def-key contruct to allow for simultaneous defining of
;; specs and docs, this will also check for duplicate keys

;; ** meta data on keyswords

;; our own private registry for meta data on namespaced keys
;; we will be adding docs to keys this way along with other
(defonce ^:private registry-ref (atom {}))

(defn key-meta [k]
  (get @registry-ref k))

(defn key-vary-meta [k f & args]
    {:pre [(keyword? k) (namespace k)]}
  (swap! registry-ref #(apply update-in % [k] f args)))

(defn key-doc
  "Given a namespaced key returns a String of documentation if it has been provided"
  [k] (-> k key-meta ::doc))

(defn add-key-doc
  "Adds documentation to a namespaced key"
  [k doc]
  {:pre [(string? doc)]}
  (key-vary-meta k assoc ::doc doc))

(defn fetch-docs
  "Given a collection of keywords return a map from those keys to their documentation"
  [kys]
  (->>  kys
       (keep (fn [k] (when-let [d (key-doc k)]
                       [k d])))
       (into {})))

;; ** Checking for duplicate keys

;; As a spec gets larger its very easy to duplicate a key and break
;; everything. It's nice to have a safe guard in place.

;; can use this to test for duplicate definitions This works fine with
;; a reloading workflow as long as you reload the whole file.

(defn duplicate-key-check* [k]
  (when (-> k key-meta ::defined)
    (throw (ex-info "Error duplicate key spec" {:k k})))
  (key-vary-meta k assoc ::defined true)
  k)

;; a development helper
;; could take a namespace ??
(defn reset-duplicate-keys
  "Empties the duplicate keys atom. Only intended for interactive development."
  []
  (doseq [[k v] @registry-ref]
    (key-vary-meta k dissoc ::defined)))

;; ** the def-key macro

;; the spec to parse out the def-key macro arguments
(def def-key-arg-spec (s/cat :k (s/and keyword? namespace)
                             :spec ::s/any
                             :doc (s/? non-blank-string?)
                             :meta-data
                             (s/* (s/cat :ky (s/and keyword? namespace)
                                         :val ::s/any))))

(defmacro def-key
  "Defines a spec via clojure.spec/def, checks for duplicates, adds
  optional documentation and optional meta data.

Usage:
(def-key ::my-keyword-key integer? \"Optional documentation\")

You can also provide extra meta-data 
(def-key ::my-keyword-key integer? \"Optional documentation\"
         ::example-data 1)"
  [& args]
  (let [{:keys [k spec doc meta-data] :as res} (s/conform def-key-arg-spec args)]
    (if-not (and k spec)
      (throw (ex-info
              (with-out-str (s/explain def-key-arg-spec args))
              (or (s/explain-data def-key-arg-spec args) {})))
      `(do
         (duplicate-key-check* ~k)
         (s/def ~k ~spec)
         ~(when doc
            `(add-key-doc ~k ~doc))
         (doseq [m# ~meta-data]
           (key-vary-meta ~k assoc (:ky m#) (:val m#)))))))

;; * finding which keys to document
;; Given an prepared error we need to find
;; a key that has documentation that is relevant to the
;

(defn search-for-key-in-via [via k]
  (some #(when (or (= (name %) (name k))
                   (= k %))
           %) (reverse via)))

;; TODO this could be much better with ::error-path
(defn find-key-to-document [e]
  (let [{:keys [via in]} e]
    (->> (reverse in)
         (filter keyword?)
         (some #(search-for-key-in-via via %)))))

(defn look-up-ns-keywords-in-spec
  "Fetches the child ns-keys of a spec given the un-namespaced keys."
  [typ kys]
  (when-let [desc (try (s/describe typ) (catch Throwable e nil))]
    (when-let [path-elems (not-empty (parse/possible-child-keys typ))]
      (->> path-elems
           (filter (fn [{:keys [ky ky-spec]}] ((set kys) ky)))
           (map :ky-spec)))))

;; let errors define how documentation gets looked
;; different errors will need different strategies
(defmulti keys-to-document ::error-type)

(defmethod keys-to-document :default [e]
  (or (::document-keys e)
      (when-let [k (find-key-to-document e)]
        [k])))

;; * moving from errors to messages

;; spec returns what we will consider raw errors
;; let's interpret these errors to provide more
;; semantic value

;; ** error type graph

;; Use derive and friends to create a tree of error types

;; We will use this tree to provide a graph of predicates to
;; conditionally upgrade a given error to a next level error

;; by using derive and providing ordering by eliminating siblings
;; have are creating a very extensible system where other libs
;; can introduce errors into the heirarchy

(def sibling-elimination (atom {}))

(defn eliminate-siblings
  "This is a mechanism to state that certain sibling type must fail
  before trying this type."
  [typ & to-eliminate]
  (assert (keyword? typ))
  (assert (namespace typ))
  (assert (every? #(and (keyword? %)
                        (namespace %)) to-eliminate))
  (swap! sibling-elimination assoc typ (set to-eliminate)))

(defn order-siblings-by-elimination
  "This is a graph sorting of siblings based on what the require to
  have happened first"
  [siblings]
  (if (= 1 (count siblings))
    siblings
    (first
     (for [sib siblings
           :let [rest-sibs (remove #(= sib %) siblings)
                 eliminations (set (apply concat
                                          (keep @sibling-elimination rest-sibs)))]
           :when (not (eliminations sib))
           :let [ordering (order-siblings-by-elimination rest-sibs)]
           :when ordering]
       (cons sib ordering)))))

(defn derived-children [typ]
  (->> @#'clojure.core/global-hierarchy
       :parents
       (filter (fn [[k v]] (v typ)))
       (map first)))

(defn children-in-order [typ]
  (let [desc (derived-children typ)]
    (reverse (order-siblings-by-elimination desc))))

(def root-error-type ::bad-value)

(defn total-order
  "this is used to give precedence to the display of errors"
  ([] (cons root-error-type (total-order root-error-type)))
  ([root-type]
   (let [desc (children-in-order root-type)]
     (concat desc (mapcat total-order desc)))))

;; ** upgrade multimethod
;; this is essentially a predicate when given a type such as ::bad-value
;; and an error it will return an error decorated with type info or nil
;; if this error doesn't match this type

;; we are adding decorations here because some of these checks are
;; pretty expensive

(defmulti upgrade-to-error-type?
  "Takes an spec based problem and an error-type and returns

An upgraded error if the error is a match
nil if no match 
:try-descedants if this error is in a chain of errors 
to try in order
"
  (fn [error-type err] error-type))

;; now we can simply go through the tree of errors types
;; this can be upgraded to skip errors in an skip-errors set

;; TODO fill this in
;; a notion that we can easily skip certain error types
;; not implemented yet
(def ^:dynamic *skip-errors-types* #{})

(defn upgrade-error* [error-type e]
  (when-let [upg-err (upgrade-to-error-type? error-type e)]
    (let [descend (children-in-order error-type)]
      (or (some #(upgrade-error* % upg-err) descend)
          upg-err))))

;; we always upgrade errors from the root
;; if the ::error-type isn't found return the original error

(defn upgrade-error [e]
  (or (upgrade-error* root-error-type e)
      e))


;; * error path

;; error paths are interesting for several reasons
;; - one is that currently the :in path returned by spec/explain-data can be very
;;   hard to navigate, so we must fix it
;; - another reason is that an error path can either point to a value, or in the
;;   case of a map it can point to a malformed key

;; we will have a notion of an error path, that we will add to the raw error

;; error-path-type
;; { :in-path [] ;; a vector path that works with get-in on the root datastructure 
;;   :error-focus :value ;; or :key
;;   ;; missing key isn't necessary we can tell if the collection doesn't have it
;;   :missing-key true } 

;; ** TODO make a spec for error-path

(defn fix-path [err]
  (if-let [pth (fix-paths/fix-error-path err (:strictly-specking.core/root-data err))]
    pth
    (:in err)))

;; error path returns the error path type for the error
(defmulti error-path ::error-type)

(defn default-error-path [err]
  {:in-path (fix-path err)
   :error-focus :value})

(defmethod error-path :default [err]
  (default-error-path err))

(defn error-path-parent
  "Given an error with an ::error-path return the path to the parent collection
of thie error element."
  [e]
  (-> e ::error-path :in-path butlast (or [])))

(defn error-key
  "Given an error with an ::error-path returh the key that this error is on."
  [e]
  (-> e ::error-path :in-path last))

(defn error-focus [e]
  (-> e ::error-path :error-focus))

;; * error definitions

;; ** TODO go over error message base cases especially when there is no parent collection
;; ** TODO see how much the error path can inform the message text

;; ** bad-value error

(defmethod upgrade-to-error-type? ::bad-value [_ {:keys [error-type] :as err}]
  (when (nil? (::error-type err))
    (assoc err ::error-type ::bad-value)))

;; printing a bad value message

(defn parent-collection [e]
  (when-let [par-path (error-path-parent e)]
    (get-in (::root-data e) par-path)))

(defn bad-value-message [{:keys [path pred val reason via in] :as e}]
  (let [ky  (error-key e)
        ;; I don't think its possible for this to be nil
        pth (error-path-parent e)
        parent-coll  (parent-collection e)]
    (cond
      (map? parent-coll)
      (str "The key " (color (pr-str ky) :focus-key)
           " at "     (color (pr-str pth) :focus-path)
           " has a non-conforming value: " (ep/format-bad-value val)
           "\n" (ep/format-predicate-str e))
      ;; handle sequential collections differently
      (coll? parent-coll)
      (str "The " (ep/type-str parent-coll)
           " at " (color (pr-str pth) :focus-path)
           " contains a non-conforming value: " (ep/format-bad-value val)
           "\n" (ep/format-predicate-str e))
      :else
      (str "The key " (color (pr-str ky) :focus-key)
           " has a non-conforming value: " (ep/format-bad-value val)
           "\n" (ep/format-predicate-str e)))))

(defmethod error-message ::bad-value [e] (bad-value-message e))
(defmethod error-message ::bad-value-comb-pred [e] (bad-value-message e))

(defmethod inline-message ::bad-value [e]
  (str "The value at key " (error-key e) " doesn't conform"))
(defmethod inline-message ::bad-value-comb-pred [e]
  (str "The value at key " (error-key e) " doesn't conform"))

(defmethod pprint-inline-message :default [e]
  (ep/pprint-in-context e (error-path-parent e)
                        {(error-key e) (inline-message e)}
                        ;; can change the colors based on focus
                        (let [focus (error-focus e)]
                          (if (= :key focus)
                            {:key-colors   [:error-key]
                             :value-colors [:none]}
                            {:key-colors   [:highlight]
                             :value-colors [:bad-value]}))))

;; ** attach reason error
(derive ::attach-reason ::bad-value)

;; this is basically a no-op
;; we could simply the ::error-type in the macro

(defmethod upgrade-to-error-type? ::attach-reason [_ err]
  (when (::attach-reason err)
    (assoc err ::error-type ::attach-reason)))

(defmethod error-path ::attach-reason [{:keys [val focus-key] :as err}]
  (if focus-key
    {:in-path (concat (fix-path err)
                      (list focus-key))
     :error-focus :key
     :missing-key (not (get val focus-key))}
    (default-error-path err)))

(defmethod error-message ::attach-reason [e]
  (str "Error at "
       (color (pr-str (vec (:in-path (::error-path e)))) :focus-path) "\n"
       (:reason e "")))

(defmethod inline-message ::attach-reason [e]
  (:reason e " error here"))

(defmethod keys-to-document ::attach-reason [e]
  (when-let [typ (last (:via e))]
    (look-up-ns-keywords-in-spec typ (when-let [x (:focus-key e)]
                                       [x]))))

;; ** wrong-size-collection

(derive ::wrong-size-collection ::bad-value)

;; TODO this needs a test to make sure that it doesn't change
;; from under us
(defn wrong-size-pred? [pred]
  (when (and (sequential? pred)
             (sequential? (nth pred 2 nil)))
    (let [[cc<=' min-v [cccount' _] max-v] pred]
      (when (and (= cc<=' 'clojure.core/<=)
                 (= cccount' 'clojure.core/count))
        {::min-count min-v
         ::max-count max-v}))))

(defmethod upgrade-to-error-type? ::wrong-size-collection [_ {:keys [pred] :as err}]
  (when-let [merge-data (wrong-size-pred? pred)]
    (-> err
        (merge merge-data)
        (assoc ::error-type ::wrong-size-collection))))

(defn pluralize [w i]
  (if (= 1 i) w (str w "s")))

(defn it-should-have-at-least [i w]
  (str "It should have at least " (color i :good) " "
       (pluralize w i)))

(defmethod error-message ::wrong-size-collection [{:keys [path pred val reason via in] :as e}]
  ;; the base case here is an
  (str "The collection at key "
       (error-key e)
       " is too small it only contains " (color (count (:val e)) :bad-value) " "
       (pluralize "value" (count (:val e))) "\n"
       (it-should-have-at-least (::min-count e) "value") "\n"))

(defmethod inline-message ::wrong-size-collection [e]
  (str "The collection is too small"))

(comment
  (let [data {:data-key []}
        errs (s/explain-data (s/map-of keyword? (s/every integer? :min-count 2))
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )

;; ** wrong-count-collection

(derive ::wrong-count-collection ::bad-value)

(defn wrong-count-pred? [pred]
  (when (and (sequential? pred)
             (sequential? (nth pred 2 nil)))
    (let [[=' target-count [count' _]] pred]
      (when (and (integer? target-count)
                 (= 'clojure.core/count count')
                 (= 'clojure.core/=     ='))
        {::target-count target-count}))))

(defmethod upgrade-to-error-type? ::wrong-count-collection [_ {:keys [pred] :as err}]
  (when-let [merge-data (wrong-count-pred? pred)]
    (-> err
        (merge merge-data)
        (assoc ::error-type ::wrong-count-collection))))

(defmethod error-message ::wrong-count-collection [{:keys [path pred val reason via in] :as e}]
  ;; the base case here is an
  (str "The collection at key "
       (error-key e)
       " has the wrong size: " (color (count (:val e)) :bad-value) "\n"
       "The count should be = to " (color (::target-count e) :good)))

(defmethod inline-message ::wrong-count-collection [e]
  (str "The collection should have exactly " (::target-count e) " " (pluralize "value" (::target-count e))))

(comment
  (let [data {:data-key []}
        errs (s/explain-data (s/map-of keyword? (s/every integer? :count 2))
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )


;; ** should-not-be-empty

(derive ::should-not-be-empty ::wrong-size-collection)

(defmethod upgrade-to-error-type? ::should-not-be-empty [_ err]
  (when (and
         (= (::min-count err) 1)
         (= (::max-count err) 'Integer/MAX_VALUE))
    (assoc err ::error-type ::should-not-be-empty)))

(defmethod error-message ::should-not-be-empty [{:keys [path pred val reason via in] :as e}]
  (str "The value " (color (pr-str val) :bad-value)
       " at key " (color (error-key e) :focus-key)
       " should not be empty.\n"
       (it-should-have-at-least 1 "value")))

(defmethod inline-message ::should-not-be-empty [e]
  (str "The value at key " (error-key e) " should not be empty"))

(comment
  (let [data {:data-key []}
        errs (s/explain-data (s/map-of keyword? (s/every integer? :min-count 1))
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )


;; ** missing required keys
;; 

(derive ::missing-required-keys ::bad-value)

(defn missing-keys? [pred]
  (when ((every-pred sequential?
                     #(= (first %) 'contains?)
                     #(keyword? (last %))) pred)
    {::missing-keys [(last pred)]}))

(defmethod upgrade-to-error-type? ::missing-required-keys [_ {:keys [pred] :as err}]
  (when-let [merge-data (missing-keys? pred)]
    (-> err
        (merge merge-data)
        (assoc ::error-type ::missing-required-keys))))

(defmethod error-path ::missing-required-keys [err]
  {:in-path (concat (fix-path err)
                    (take 1 (::missing-keys err))) 
   :error-focus :key
   :missing-key true})

(defmethod error-message ::missing-required-keys [{:keys [path pred val reason via in] :as e}]
  (when-let [kys (not-empty (::missing-keys e))]
    (str "Missing required " (pluralize "key" (count kys)) " "
         (ep/format-seq-with-and kys) " at path "
         (color (pr-str (error-path-parent e)) :focus-path))))

(defmethod pprint-inline-message ::missing-required-keys [e]
  (or
   (and (::file-source e) (ep/pprint-missing-keys-in-file-context e))
   (ep/pprint-sparse-path (::root-data e)
                          (error-path-parent e)
                          (into {}
                                (map (fn [k]
                                       [k (str "The required key " (pr-str k) " is missing")]))
                                (::missing-keys e)))))

(defmethod keys-to-document ::missing-required-keys [e]
  (when-let [missing (not-empty (::missing-keys e))]
    (let [typ  (last (:via e))]
      (look-up-ns-keywords-in-spec typ missing))))

(comment
  (let [data {}
        errs (s/explain-data (strict-keys :req-un [::asdf])
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )


;; ** bad key
;; this is a case where we have a s/map-of or s/every-kv

(derive ::bad-key ::bad-value)

(eliminate-siblings ::bad-key ::unknown-key) ;; if this isn't an unknown key

;; we are using :in on purpose here 
(defmethod upgrade-to-error-type? ::bad-key [_ {:keys [in val] :as err}]
  (when (= [0 val] (take 2 (reverse in)))
    (assoc err ::error-type ::bad-key)))

(defmethod error-path ::bad-key [err]
  (-> (default-error-path err)
      (assoc :error-focus :key)))

(defn bad-key-message [{:keys [path pred val reason via in] :as e}]
  (str "The key " (color (pr-str (error-key e))
                         :focus-key)
       " at " (color (pr-str (error-path-parent e)) :focus-path)
       " does not conform. "
       "\n" (ep/format-predicate-str e)))

(defmethod error-message ::bad-key [e]           (bad-key-message e))
(defmethod error-message ::bad-key-comb-pred [e] (bad-key-message e))

(defn bad-key-inline-message [e]
  (str "The key " (pr-str (error-key e))
       " does not conform."))

(defmethod inline-message ::bad-key [e] (bad-key-inline-message e))
(defmethod inline-message ::bad-key-comb-pred [e] (bad-key-inline-message e))

(comment
  (let [data {"" 1
              3 3}                               ;; below is for combined key errors
        errs (s/explain-data (s/map-of integer? #_(s/or :key keyword? :int integer?)
                                       integer?)
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )



;; NEXT level is ::unknown-key problems



;; ** unknown key

(derive ::unknown-key ::bad-value)

;; this only works with the strict-keys implementation
;; TODO think about getting rid of the strict keys complexity
;; and just pulling the data out of a pred
(defmethod upgrade-to-error-type? ::unknown-key [_ err]
  (when (::unknown-key err)
    (assoc err ::error-type ::unknown-key)))

(defmethod error-path ::unknown-key [err]
  {:in-path (concat (fix-path err)
                    (list (::unknown-key err)))
   :error-focus :key})

(defmethod error-message ::unknown-key [{:keys [path pred val reason via in] :as e}]
  (str "Found unrecognized key " (color (pr-str (::unknown-key e)) :error-key)
       " at path " (color (pr-str (error-path-parent e)) :focus-path) "\n"
       ;; doesn't work when there is only one key
       (if (= 1 (count pred))
         (str "The only allowed key is: " (pr-str (first pred)) )
         (str "Must be one of: " (ep/format-seq-with-or pred)))))

(defmethod inline-message ::unknown-key [e]
  (str "The key " (::unknown-key e) " is unrecognized"))

(defmethod keys-to-document ::unknown-key [e] nil)

(comment
  (let [data {:abcd 1}                               
        errs (s/explain-data (strict-keys :opt-un [::fargo])
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil))
  )


;; ** misspelled-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys

(derive ::misspelled-key ::unknown-key)

(defmethod upgrade-to-error-type? ::misspelled-key [_ err]
  (when-let [suggest
             (and
              (::keys->specs err)
              ((some-fn keyword? string? symbol?) (::unknown-key err))
              (strict-impl/spelling-suggestion
               (parse/keys-specs->keys-args (::keys->specs err))
               (:val err)
               (::unknown-key err)))]
    (assoc err
           ::error-type ::misspelled-key
           ::misspelled-key (::unknown-key err)
           ::correct-key suggest)))

(defmethod error-message ::misspelled-key [{:keys [path pred val reason via in] :as e}]
  (str "It's likely that the key " (color (pr-str (::misspelled-key e)) :error-key)
       " is misspelled. It should probably be "
       (color (pr-str (::correct-key e)) :correct-key)))

(defn correct-key-message [e]
  (str "The key " (pr-str (error-key e))
       " should probably be " (pr-str (::correct-key e))))

(defmethod inline-message ::misspelled-key [e]
  (correct-key-message e))

(defn correct-keys-to-document [e]
  (when-let [spec-key (get (::keys->specs e)
                           (::correct-key e))]
    [spec-key]))

(defmethod keys-to-document ::misspelled-key [e]
  (correct-keys-to-document e))

(comment
  (let [data {:farg 1}                               
        errs (s/explain-data (strict-keys :opt-un [::fargo])
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil)
    (keys-to-document (first (prepare-errors errs
                                             data
                                             nil)))
    )
  )

;; ** wrong-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys
;; but the scores of the top choices should be close and in order 

(derive ::wrong-key ::unknown-key)

;; depends on previous error-upgrade failing
(eliminate-siblings ::wrong-key ::misspelled-key)

(defmethod upgrade-to-error-type? ::wrong-key [_ err]
  (when-let [suggest
             (and
              (::keys->specs err)
              (strict-impl/replacement-suggestion
               (parse/keys-specs->keys-args (::keys->specs err))
               (:val err)
               (::unknown-key err)))]
    (assoc err
           ::error-type ::wrong-key
           ::wrong-key (::unknown-key err)
           ::correct-key suggest)))

(defmethod error-message ::wrong-key [{:keys [path pred val reason via in] :as e}]
  (str "The key " (color (pr-str (error-key e)) :error-key)
       " is unrecognized. Perhaps you meant "
       (color (pr-str (::correct-key e)) :correct-key)
       "?"))

(defmethod inline-message ::wrong-key [e]
  (correct-key-message e))

(defmethod keys-to-document ::wrong-key [e]
  (correct-keys-to-document e))

(comment
  (let [data {:forest {:id "asdf"
                       :source-paths ["src"]
                       :compiler {:output-to "main.js"}}}
        errs (s/explain-data (strict-keys
                              :opt-un [:strictly-specking.test-schema/build-config])
                             data)]
    (dev-print errs
               data
               nil)
    (prepare-errors errs
                    data
                    nil)

    (keys-to-document (first (prepare-errors errs
                    data
                    nil)))

    )
  
  )

;; ** misplaced key
;; 

(derive ::misplaced-key ::unknown-key)

(eliminate-siblings ::misplaced-key ::misspelled-key ::wrong-key)

(defn misplaced-key-error? [err]
  ;; need the root
  ;; perhaps the first of :via??
  (when-let [ky (and (first (:via err)) (::unknown-key err))]
    (when-let [possible-paths
               (not-empty
                (set
                 (path-match/filter-possbile-path-choices
                  err
                  (parse/find-key-path-without-ns (first (:via err)) ky))))]
      (let [[suggested-path-reified
             suggested-path] (path-match/best-possible-path possible-paths (::root-data err))]
        {::suggested-path suggested-path-reified
         ::document-keys [(-> suggested-path last :ky-spec)]}))))

(defmethod upgrade-to-error-type? ::misplaced-key [_ err]
  (when-let [merge-data (misplaced-key-error? err)]
    (-> err
        (merge merge-data)
        (assoc ::error-type ::misplaced-key))))

(defmethod error-message ::misplaced-key [e]
  (str "The key " (color (pr-str (::unknown-key e)) :focus-key)
       " at " (color (pr-str (error-path-parent e)) :focus-path)
       " is on the wrong path."))

(defmethod inline-message ::misplaced-key [e]
  (str "The key " (pr-str (-> e ::unknown-key)) " has been misplaced"))

(defmethod keys-to-document ::misplaced-key [e]
  (::document-keys e))

;; ** misplaced misspelled key
;; 

(derive ::misplaced-misspelled-key ::unknown-key)

(eliminate-siblings ::misplaced-misspelled-key ::misplaced-key ::misspelled-key ::wrong-key)

(defn suggested-misspelled-paths [e]
  (let [res (not-empty (parse/find-key-path-like-key (first (:via e)) (::unknown-key e)))]
    (mapv #(vector
            %
            (similar-key (-> % last :ky)
                         (::unknown-key e))) res)))

#_(suggested-misspelled-paths {::unknown-key :source-m
                             :val ["src"]
                             :via [:strictly-specking.test-schema/lein-project-with-cljsbuild]})

(defn score-suggested-path [e [suggested-key-path score]]
  (let [{:keys [ky ky-spec]} (last suggested-key-path)
        key-val (get (:val e) (::unknown-key e))
        valid (and (parse/spec-from-registry ky-spec)
                   (s/valid? ky-spec key-val))]
    [suggested-key-path
     (cond-> (/ score 10)
       ;; TODO: I don't think this is neccessary at all
       (and valid (coll? key-val)) dec
       valid (- 1/10) ;; a single letter difference for being an atomic value and correct
       (and (not valid) (map? key-val) (strict-impl/fuzzy-spec? ky-spec))
       (- (or (strict-impl/fuzzy-conform-score (parse/spec-from-registry ky-spec) key-val)
              0)))]))

(defn sort-suggestions [e]
  (let [res (sort-by
             last
             (mapv
              (partial score-suggested-path e)
              (suggested-misspelled-paths e)))
        first-score (-> res first last)]
    (map first
         (take-while #(= first-score (last %))
                     res))))

#_(sort-suggestions {::unknown-key :source-m
                     :val {:source-m true}
                     :via [:strictly-specking.test-schema/lein-project-with-cljsbuild]})

(defn misplaced-misspelled-key-error? [err]
  ;; need the root
  ;; perhaps the first of :via??
  (when-let [ky (and (first (:via err)) (::unknown-key err))]
    (when-let [possible-paths (not-empty (set (sort-suggestions err)))]
      (let [[suggested-path-reified
             suggested-path] (path-match/best-possible-path possible-paths (::root-data err))]
        {::suggested-path suggested-path-reified
         ::correct-key (-> suggested-path last :ky)
         ::document-keys [(-> suggested-path last :ky-spec)]}))))

(defmethod upgrade-to-error-type? ::misplaced-misspelled-key [_ err]
  (when-let [merge-data (misplaced-misspelled-key-error? err)]
    (-> err
        (merge merge-data)
        (assoc ::error-type ::misplaced-misspelled-key))))

(defmethod error-message ::misplaced-misspelled-key [e]
  (str "The key " (color (pr-str (::unknown-key e)) :focus-key)
       " at " (color (pr-str (error-path-parent e)) :focus-path)
       " is misspelled and on the wrong path."))

(defmethod inline-message ::misplaced-misspelled-key [e]
  (str "The key " (pr-str (-> e ::unknown-key)) " has been misspelled and misplaced"))

(defmethod keys-to-document ::misplaced-misspelled-key [e]
  (::document-keys e))

;; * error combination

;; ** combine errors for single location
;; Sometimes there are multiple errors on the same path location

;; TODO :in is still being used here,
;; need to verify that it isn't causing problems
(defn combined-or-pred [errors]
  (let [errors-to-comb
        (->> errors
             (filter #(= (::error-type %) ::bad-value))
             (group-by :in)
             (filter #(< 1 (count (second %))))
             (map (fn [[_ errors]]
                    (assoc (first errors)
                           :pred (cons 'or (map :pred errors))
                           :path (cons ::combined-path (map :path errors))
                           ::error-type ::bad-value-comb-pred))))
        paths (set (map :in errors-to-comb))]
    (concat (filter #(not (paths (:in %))) errors)
            errors-to-comb)))

(defn combined-or-key-pred [errors]
  (let [errors-to-comb
        (->> errors
             (filter #(= (::error-type %) ::bad-key))
             (group-by (comp :in-path ::error-path))
             (filter #(< 1 (count (second %))))
             (map (fn [[_ errors]]
                    (assoc (first errors)
                           :pred (cons 'or (map :pred errors))
                           :path (cons ::combined-path (map :path errors))
                           ::error-type ::bad-key-comb-pred))))
        paths (set (map (comp :in-path ::error-path) errors-to-comb))]
    (concat (filter #(not (paths (-> % ::error-path :in-path))) errors)
            errors-to-comb)))

;; ** combine missing-keys errors
;; missing keys for the same location can be combined

(defn combined-missing-keys [errors]
  (let [errors-to-comb
        (->> errors
             (filter #(= (::error-type %) ::missing-required-keys))
             (group-by :in)
             (filter #(< 1 (count (second %))))
             (map (fn [[_ errors]]
                    (assoc (first errors)
                           :pred (cons 'and (map :pred errors))
                           ::missing-keys (vec (mapcat ::missing-keys errors))))))
        paths (set (map :in errors-to-comb))]
    (concat (filter #(not (paths (:in %))) errors)
            errors-to-comb)))

;; ** corrections overide missing
;; if there is an error with a ::correct-key in it eliminate any ::missing-required-keys reference
;; TODO still using in here, need to review
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

;; ** specific paths filter out general paths if something fails
;; at a deeper path it takes precedence over and filters out more
;; general paths this implies that the more general path succeeded in
;; a certain branch and that path is the one that is failing

(defn sub-path? [sub-path* path]
  (and (<= (count sub-path*) (count path))
       (reduce #(and %1 %2) true (map = path sub-path*))))

(defn specific-paths-filter-general [errors]
  (->> errors
       (sort-by (comp - count :in-path ::error-path))
       (reduce (fn [accum err]
                 (let [in-path (:in-path (::error-path err))]
                   (if (some #(sub-path? in-path %) (keys accum))
                     accum
                     (assoc accum in-path err)))) {})
       vals))

;; * sorting errors
;;  sort them by graph depth

(defn sort-errors [errors]
  (let [order (vec (reverse (total-order)))]
    (sort-by
     (fn [v] (let [pos (.indexOf order (::error-type v))]
               (if (neg? pos) 10000 pos))) errors)))

;; * prepare errors
;; this brings all the functionality above together

;; this is still rough
;; need a notion of a source ;; pure data vs. file source

(defn prepare-errors [explain-data validated-data file]
  (->> explain-data
       ::s/problems
       #_filter-errors
       (map #(assoc % ::root-data validated-data))
       (map #(if file (assoc % ::file-source file) %))
       (map upgrade-error)
       #_fix-map-paths
       
       #_upgrade-to-misplaced-errors
       sort-errors
       (map #(assoc % ::error-path (error-path %)))
       combined-missing-keys
       combined-or-pred
       combined-or-key-pred
       corrections-overide-missing-required
       specific-paths-filter-general))

;; * error to final display data

(defmulti update-display-data (comp ::error-type :error))

(defmethod update-display-data :default [disp-data] disp-data)

;; parameterize the key and messages
(defn misplaced-update-data [disp-data]
  (let [err (:error disp-data)
        ky  (or (::correct-key err) (::unknown-key err))]
    (assoc disp-data
           :extra-explain
           (if (::correct-key err)
             (str "The key " (pr-str (::unknown-key err))
                  " probably needs to be changed to " (pr-str ky)
                  " and placed on a path like: ")
             (str "The " (pr-str ky)
                  " key should probably be placed like so: "))
           :extra-diagram (with-out-str
                            (ep/pprint-sparse-path
                             (path-match/generate-path-structure
                              (::root-data err)
                              (::suggested-path err)
                              (get (:val err)
                                   (::unknown-key err)))
                             ;; very unhappy with this
                             (path-match/generate-path-structure-path
                              (::root-data err)
                              (butlast (::suggested-path err)))
                             {ky
                              (if (::correct-key err)
                                (str "The correctly spelled key should have a path like this")
                                (str "The key " ky
                                     " should probably be placed here"))}
                             {:key-colors [:good]})))))

(defmethod update-display-data ::misplaced-key [disp-data]
  (misplaced-update-data disp-data))

(defmethod update-display-data ::misplaced-misspelled-key [disp-data]
    (misplaced-update-data disp-data))

;; TODO the raw line data may be more appropirate to ship over
;; the wire to a client
(defn error->display-data [error]
  (update-display-data
   {:error error
    :message (error-message error)
    :error-in-context (with-out-str (pprint-inline-message error))
    :path (-> error ::error-path :in-path)
    :doc-keys (keys-to-document error)
    :docs (not-empty (fetch-docs (keys-to-document error)))}))

;; * actually printing of the error

(defn test-print [error-data]
  (println (color "---------------------------------\n" :header))
  (println (:message error-data))
  (println "\n")
  ;; could should indent this
  (println (:error-in-context error-data))
  (println "\n")
  (when (:extra-explain error-data)
    (println (:extra-explain error-data))
    (println "\n"))
  (when (:extra-diagram error-data)
    (println (:extra-diagram error-data))
    (println "\n"))
  (when (:extra-extra-explain error-data)
    (println (:extra-extra-explain error-data))
    (println "\n"))
  #_(println "Docs " (prn (:doc-keys error-data)))
  (when (not-empty (:docs error-data))
    (doseq [[ky doc] (:docs error-data)]
      (println "-- Docs for key" (pr-str (keyword (name ky)))"--")
      (println doc)
      (println "\n")))
  (doseq [[k v] error-data
          :when (not (#{:message :error-in-context :extra-explain :extra-diagram
                        :extra-extra-explain :docs :final-notes
                        :path :error :doc-keys}
                      k))]
    (println "--" (pr-str k) "--")
    (println v)
    (println "\n"))
  (when (:final-notes error-data)
    (println (:final-notes error-data))
    (println "\n"))
  (println (color "---------------------------------\n" :footer)))

(defn dev-print [explain-data data-to-test file-name]
  (without-ansi
    (->> 
     (prepare-errors explain-data data-to-test file-name)
     (map error->display-data)
     #_(take 1)
     (mapv test-print))))

(comment

  
  (let [d {:cljsbuild
           {:builds
            [{:source-paths ["src"]
              :compiler
              {:figwheel
               {:websocket-host "localhost"
                :on-jsload      'example.core/fig-reload
                :on-message     'example.core/on-message
                :source-map true
                :debug true}}}]}}]
    (dev-print
     (s/explain-data :strictly-specking.test-schema/lein-project-with-cljsbuild d) d nil))
  
  (def data-from-file (read-string (slurp "dev-resources/test_edn/cljsbuild.edn")))

  (dev-print
   (s/explain-data :strictly-specking.test-schema/lein-project-with-cljsbuild
                   data-from-file)
   data-from-file
   "dev-resources/test_edn/cljsbuild.edn")

  (def errr (first (prepare-errors (s/explain-data :strictly-specking.test-schema/lein-project-with-cljsbuild
                                          data-from-file)
                          data-from-file
                          "dev-resources/test_edn/cljsbuild.edn")))
  


  (generate-suggested-path-data
                              errr
                              (::suggested-path errr)
                              (::unknown-key errr))

  (mapv #(path-match/path-match  %  data-from-file)
        
        
   '[#_({:ky-spec :figwheel.lein-project/figwheel, :ky :figwheel}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/int-key}
      {:ky-spec :strictly-specking.test-schema/compiler, :ky :compiler})
     ({:ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
       :ky :cljsbuild}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/pred-key,
       :ky-pred-desc :strictly-specking.test-schema/string-or-named}
      {:ky-spec :strictly-specking.test-schema/compiler, :ky :compiler})
     #_({:ky-spec :figwheel.lein-project/figwheel, :ky :figwheel}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/pred-key,
       :ky-pred-desc :strictly-specking.test-schema/string-or-named}
      {:ky-spec :strictly-specking.test-schema/compiler, :ky :compiler})
     ({:ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
       :ky :cljsbuild}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/int-key}
      {:ky-spec :strictly-specking.test-schema/compiler, :ky :compiler})]

   )



  
  

  
  )

;; * additional specs

;; ** Strict-keys Spec

(defmacro strict-keys
  "This is a spec that has the same signature as the clojure.spec/keys spec.
  The main difference is that it fails on keys that are not the 
  [:req :opt :req-un :opt-un] specifications.

  This spec will provide an explanation for each unknown key."
  [& args]
  ;; check the args with s/keys
  ;; `(s/keys ~@args)
  (let [form (macroexpand `(s/keys ~@args))]
    `(strictly-specking.strict-keys/strict-mapkeys-impl
      (strictly-specking.parse-spec/parse-keys-args ~@args) ~form)))


;; ** Attach Reason Spec

;; attach-reason is a spec that attaches a :reason to a failing specs
;; explain-data this :reason is printed out as part of the message
;; generated by clojure.spec/explain

(defn ammend-explain-impl [parent-spec {:keys [reason] :as additional-info}]
  {:pre [(or (nil? reason) (string? reason))]}
  (reify
    clojure.lang.IFn
    (invoke [this x] (parent-spec x))
    clojure.spec/Spec
    (conform* [_ x] (s/conform* parent-spec x))
    (unform* [_ x] (s/unform* parent-spec x))
    (explain* [_ path via in x]
      (when-let [err (first (s/explain* parent-spec path via in x))]
        [(cond-> err
           (::attach-reason additional-info)
           (update-in [:reason] #(if %
                                   (str (::attach-reason additional-info) "\n - " %)
                                   (::attach-reason additional-info)))
           true (merge additional-info))]))
    ;; These can be improved
    (gen* [_ a b c]
      (s/gen* parent-spec a b c))
    (with-gen* [_ gfn]
      (s/with-gen* parent-spec gfn))
    (describe* [_] (s/describe* parent-spec))))

(defmacro attach-reason [reason spec & additional-info]
  `(ammend-explain-impl (s/spec ~spec)
                        ~(into {::attach-reason reason}
                               (apply hash-map additional-info))))

#_(s/explain-data (attach-reason "asdf" (fn [x] false)
                               :focus-key :asdf)
                {})

;; ** Attach Warning Spec

;; There are situations that we just want to warn about. Here is a spec
;; that will print out a warning and will always pass.

(defn attach-warning-impl [message-or-fn parent-spec]
  {:pre [(or (fn? message-or-fn) (string? message-or-fn))]}
  (reify
    clojure.lang.IFn
    (invoke [this x] (parent-spec x))
    clojure.spec/Spec
    (conform* [_ x]
      (if (s/valid? parent-spec x)
        (s/conform* parent-spec x)
        (do
          (println "Spec Warning: " (if (string? message-or-fn)
                                      message-or-fn
                                      (message-or-fn x)))
          x)))
    (unform* [_ x] x)
    (explain* [_ path via in x] nil)
    ;; These can be improved
    (gen* [_ a b c]
      (s/gen* parent-spec a b c))
    (with-gen* [_ gfn] 
      (s/with-gen* parent-spec gfn))
    (describe* [_] (cons 'attach-warning (s/describe* parent-spec)))))

(defmacro attach-warning [reason spec]
  `(attach-warning-impl ~reason (s/spec ~spec)))







;; * thoughts
;; ** Data needs for spec failure error expression
;; *** top level needs
;; **** display the error in situ

;; its important the display the error in it the context of its
;; original data structure

;; this is more powerful if the data structure is a in source form
;; such as edn in a file

;; not having an edn structure we are only left with displaying it
;; in the context of pure data.

;; its also important to be able to display relevant documentation
;; for various keys involved in the error

;; This brings up the following needs:
;; ***** the original data structure
;; ***** the source be it file or some string
;; ***** the path to the error
;; this path needs to work function with the original data structure
;; ***** when the last element of the path is a key to a map
;; we need to know if it points to the value or the key itself

;; **** types of errors that can occur
;; ***** non conforming value error
;; the value at the path does not comply

;; A value error needs
;; - the path that ends at the key that points at the value
;; - the knowledge that the path points to the value
;; - what was wrong with the value - the specs that failed
;; - the data to look up the docs for that key

;; path to error value

;; this is a global need for all errors
;; - a message that clarifies the error given the above data

;; but in a way everything is a value error at some level
;; if there is a key pointing to a value that the error is in.

;; For example if a map has an unknown-key, the map in its
;; entirety could be the bad value for a parent key
;; the some errors can be made more specific by upgrading
;; them to a more specific error.

;; -----
;; there will be more specific bad value errors
;; ::should-not-be-empty
;; ::collection-wrong-size
;; these will be available for different concrete errors
;; that caused by built in macro predicates
;; -----

;; here are some upgraded errors
;; ***** missing required key error
;; a required key is missing from the map
;;
;; this is an upgrade of a bad value error on the parent
;; need to detect this error and upgrade


;; ****** TODO
;; error-path-type
;; { :in-path [] ;; a vector path that works with get-in on the root datastructure 
;;   :error-focus :value ;; or :key
;;   :missing-key true }
;; error-parent-path always butlast of :path
;; method error-path
;; method error-docs
;; method keys-to-document




;; 
;; methods
;; - context-print-path
;;   again path itself holds notion of pointing to the key or value
;; - bad-value-path
;;   path to bad value
;; - focus-keys
;;   the keys in the collection that have the focus

;; really this notion breaks down in many cases
;; can we have an error with several erroneous keys in a single erroneous value?
;; Then we will want a notion of a path to the parent collection
;; and a notion of a set of eronneous keys
;; OR instead of combining errors a notion of merging them into the same report
;; so that we can display them in the same structure
;; part of merging would be collision


;; notion of printing path

;; needs
;; - predicate to spot this bad value error
;; - upgrade this error path by appending the missing key
;; - an indicator that the key is the focus of the error
;; - document the key that is required
;;   this means we need the ns-key to look up documentation
;; - upgrade the data-structure visualizaion to display the missing key
;;  - in the case of an edn structure add some keys and values
;;  - in the case of a file source insert some lines with the missing key values
;;
;; the strange printing needs of this error point out a need for a in
;; context printing hook

;; ***** unknown-key
;; an unknown-key is present in a map
;;
;; this is an upgrade from a bad value error

;; - need to display list of correct keys
;; - need a predicate to detect this error type
;; - need to upgrade the path:
;;   - need a path to the key
;;   - need an indicator that this is a key error
;; - *no* documentation is needed perhaps documentation on the
;; parent key

;; in a certain mode this would just be awarning

;; ***** mispelled key error
;; a key has been determined to be a likely mispelling

;; this is an upgrade from an unkown-key error
;; it will use the corrected paths

;; we need a predicate to detect this error
;;   - this predicate needs the other possible keys in the map
;;   - it needs to know the keys in the map already
;;   - it needs to be able to validate the value against the
;;     alternate key suggestions
;;   - the :val and the map of keys->ns-key-specs can provide the
;;     bulk of these data needs
;;   - can possibly use the parent-spec to find these keys (last :via)

;; need to document the suggestion

;; should really consider displaying serveral suggestions

;; should consider that this could just be a warning in a certain mode
;; the warning would warn about unknown keys and provide the possible misspellings

;; ***** wrong key error

;; very similar to the mispelled key and is only tried if no mispelling is found
;; it just determines suggestions based on the shap of the unkown keys value
;; trying to find the best match in the

;; again its an evolution of an unknown-key
;; it will reuse the path information in it

;; we need a predicate to detect this error
;;   - this predicate needs the other possible keys in the map
;;   - it needs to know the keys in the map already
;;   - it needs to be able to validate the value against the
;;     alternate key suggestions
;;   - the :val and the map of keys->ns-key-specs can provide the
;;     bulk of these data needs
;;   - can possibly use the parent-spec to find these keys (last :via)

;; this predicate would return an error modified with the correct information
;;    the name of the correct-key (we already know the focus-key)

;; need to document the suggestion(s)
;; 

;; ***** misplaced key
;; this is an error that occurs when the key likey belongs somewhere
;; else in the datastructure

;; this is an upgrade of an unknown-key

;; this is only tried if a local mispelling error and a local wrong key error
;; hasn't been detected

;; need a predicate to detect this error
;; this predicate needs to do enough computation that it's probably better to dual purpose
;; this predicate such that it returns a modified error with added information
;;   
;;

;; ***** mispelled misplaced key


;; ***** global wrong key
;; the key has a wrong value but the value has sufficietn complexity
;; and validates correrctly against another position


;; ***** wrong placement of structure
;; key has wrong value but the value has sufficient complexity
;; and belongs somewhere else

;; need to be able to turn up and down the level of validation errors
;; just listing the errors might not work because there are evolutions
;; of errors



;; **** Notes
;; looks like we may just be able to have an indicator of a :focus-shift
;; that points one more level deep in the associative structure
;; and indicates if its a key problem "or a value problem" (TODO it might never
;; be a valure problem)



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






