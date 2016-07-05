(ns strictly-specking.error-printing
  (:require
   [strictly-specking.ansi-util :as cl :refer [color]]
   [strictly-specking.annotated-pprint :as annot]
   [strictly-specking.edn-string-nav :as edn-string-nav]
   [strictly-specking.context-print :as cp]
   [clojure.string :as string]
   [clojure.pprint :as pp]))

#_(remove-ns 'strictly-specking.error-printing)

;; ** error-message function
;; this will provide a more meaningful description of whats gone wrong
;; will delegete to a ns/key based error message if available

(defn indent-lines [n s]
  (->> (string/split s #"\n")
       (map #(str (cp/blanks n) %))
       (string/join "\n")))

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
(def ^:dynamic *symbol-type-table*
  '{non-blank-string? NonBlankString
    string?  String
    symbol?  Symbol
    keyword? Keyword
    vector?  Vector
    sequential? Sequence
    map?     Map
    integer? Integer
    number?  Number})

(def type-lookup->str (some-fn *symbol-type-table* pr-str))

(defn resovlable-predicate? [x]
  (or (*symbol-type-table* x)
   (and
    (sequential? x)
    (= 1 (count x))
    (*symbol-type-table* (first x)))))

(defn pred-symbol->str [x]
  (if-let [res (resovlable-predicate? x)]
    res
    (type-lookup->str x)))

(defn seq-with-first [x pred]
  (and (sequential? pred) (= x (first pred))))

(defn format-predicate-str [{:keys [pred val]}]
  (cond
    (or
     (seq-with-first 'or pred)
     (seq-with-first 'some-fn pred))
    (str (if (every? *symbol-type-table* (rest pred))
           "It should be one of: "
           "It should satisfy one of: ")
         (string/join " | "
                      (map #(color % :good-pred)
                           (map (some-fn *symbol-type-table* str) (rest pred)))))
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

(defn type-str [t]
  (cond
    (map? t)      "Map"
    (set? t)      "Set"
    (vector? t)   "Vector"
    (sequential? t) "Sequence"))

(defn format-bad-value [val]
  (let [formatted (color (format-summarized-value val) :bad-value)]
    (if (coll? val)
      (str "\n" (indent-lines 2 formatted))
      formatted)))



;; *** TODO figure out how to identify where to point to

;; we need to abstract how we choose a key for a given error
;; might not need this yet

(defmulti error-message :strictly-specking.core/error-type)

(defmethod error-message :default [e]
  (message-default-str e #_(select-keys e [:pred :path :val :reason :via :in])))




(defn bad-key-message [{:keys [path pred val reason via in] :as e}]
  (let [k (last in)]
    (str "The key " (color (pr-str k) :focus-key)
         " at " (color (pr-str (vec in)) :focus-path)
         " does not conform. "
         "\n" (format-predicate-str e))))

(defmethod error-message ::bad-key [e]           (bad-key-message e))
(defmethod error-message ::bad-key-comb-pred [e] (bad-key-message e))

(defmethod error-message ::misplaced-key [e]
  (let [k (::unknown-key e)]
    (str "The key " (color (pr-str k) :focus-key)
         " at " (color (pr-str (vec (:in e))) :focus-path)
         " is on the wrong path. "
         "\n")))

(defmethod error-message ::attach-reason [e]
  (str "Error at "
       (color (pr-str (vec (:in e))) :focus-path) "\n"
       (:reason e "")))

(defmethod error-message ::should-not-be-empty [{:keys [path pred val reason via in] :as e}]
  (str "The value " (color (pr-str val) :bad-value)
       " at key " (color (last in) :focus-key)
       " should not be empty.\n"))

(defmethod error-message ::unknown-key [{:keys [path pred val reason via in] :as e}]
  (str "Found unrecognized key " (color (::unknown-key e) :error-key)
       " at path " (color (pr-str in) :focus-path) "\n"
       "Must be one of: " (format-seq-with-or pred)))

(defmethod error-message ::missing-required-keys [{:keys [path pred val reason via in] :as e}]
  (when-let [kys (not-empty (::missing-keys e))]
    (if (< 1 (count kys))
      (str "Missing required keys " (format-seq-with-and kys) " at path " (color (pr-str in) :focus-path))
      (str "Missing required key "  (format-seq-with-and kys) " at path " (color (pr-str in) :focus-path)))))

;; *** TODO ::wrong-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys
;; but the scores of the top choices should be close and in order 

(defmethod error-message ::wrong-key [{:keys [path pred val reason via in] :as e}]
  (str "The key " (color (::wrong-key e) :error-key)
       " is unrecognized. Perhaps you meant "
       (color (::correct-key e) :correct-key)
       "?"))

;; *** TODO ::misspelled-key
;; upon reflection misspelling and wrong keys should have multiple options for correction
;; we probably should move ::correct-key to ::correct-keys

(defmethod error-message ::misspelled-key [{:keys [path pred val reason via in] :as e}]
  (str "The key " (color (::misspelled-key e) :error-key)
       " is misspelled. It should probably be "
       (color (::correct-key e) :correct-key)))

;; *** TODO: fill in the rest of the error types

(defmulti inline-message :strictly-specking.core/error-type)

(defmethod inline-message :default [e] (str "The key " (-> e :in last) " has a problem"))

(defmethod inline-message ::misplaced-key [e]
  (str "The key " (-> e ::unknown-key) " has been misplaced"))

(defmethod inline-message ::attach-reason [e]
  (:reason e " error here"))

(defmethod inline-message ::bad-value-comb-pred [e]
  (str "The key " (-> e :in last) " has a non-conforming value"))

(defmethod inline-message ::bad-key [e]
  (str "The key " (-> e :in last) " does not conform."))

(defmethod inline-message ::bad-key-comb-pred [e]
  (str "The key " (-> e :in last) " does not conform."))

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
   (->>
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
                             key-message-map))})))
    with-out-str
    (indent-lines 2)
    println))
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
      (println "File:" (str file))
      (cp/print-message-in-context-of-file file line column message)
      true)))

#_(edn-string-nav/get-path-in-clj-file [0] "tester.edn")

(defn pprint-in-context
  ([e base-path key-message-map]
   (pprint-in-context e base-path key-message-map {}))
  ([e base-path key-message-map colors]
   (or (and
        (:strictly-specking.core/file-source e)
        (pprint-in-file (:strictly-specking.core/file-source e) base-path key-message-map))
     (pprint-sparse-path (:strictly-specking.core/root-data e) base-path key-message-map colors))))

;; *** pprint inline message
;;
;; this should dispatch and display in file context if there is file
;; information on the error

;; ***** TODO the real abstraction here is a way to determine
;; the focus-key and the path to the containing structure
;; in other words we need an abstraction of a path
;; and wether the path points to key endpoint of the path
;; or the value on the path

;; this ends up being different for each error

;; I can imagine a multimethod that returns this
;; {:path [] :focus (or :ky :vl)}
;; where the last component of the path is the focus key

(defmulti pprint-inline-message :strictly-specking.core/error-type)

(defmethod pprint-inline-message :default [e]
  (pprint-in-context e (butlast (:in e))
                     {(last (:in e)) (inline-message e)}
                     {:key-colors [:highlight]
                      :value-colors [:bad-value]}))

(defmethod pprint-inline-message ::unknown-key [e]
  (pprint-in-context e (:in e)
                     {(::unknown-key e) (inline-message e)}))

;; attach-reason provides a focus key when it's ambigious which
;; key is 
(defmethod pprint-inline-message ::attach-reason [{:keys [in focus-key] :as e}]
  (let [[path ky] (if focus-key
                    [in focus-key]
                    [(butlast in) (last in)])]
    (pprint-in-context e path
                       {ky (inline-message e)})))

(defmethod pprint-inline-message ::bad-key [e]
  (pprint-in-context e (butlast (:in e))
                     {(last (:in e)) (inline-message e)}))

(defmethod pprint-inline-message ::bad-key-comb-pred [e]
  (pprint-in-context e (butlast (:in e))
                     {(last (:in e)) (inline-message e)}))

(defmethod pprint-inline-message ::misplaced-key [e]
  (pprint-in-context e (:in e)
                     {(::unknown-key e) (inline-message e)}))

(defmethod pprint-inline-message ::misspelled-key [e]
  (pprint-in-context e (:in e)
                     {(::misspelled-key e) (inline-message e)}))

(defmethod pprint-inline-message ::wrong-key [e]
  (pprint-in-context e (:in e)
                     {(::wrong-key e) (inline-message e)}))

(defn pprint-missing-keys-context [e]
  (pprint-sparse-path (reduce #(update-in %1 (vec (:in e))
                                          (fn [x]
                                            ;; order matters for display purposes
                                            (->> x
                                                 seq
                                                 (cons [%2 '_____])
                                                 (into {}))))
                              (:strictly-specking.core/root-data e)
                              (:strictly-specking.core/missing-keys e))
                      (:in e)
                      (into {}
                            (map (fn [k]
                                   [k (str "The required key " (pr-str k) " is missing")]))
                            (:strictly-specking.core/missing-keys e))))

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
  (let [m (get-in (:strictly-specking.core/root-data e) (:in e))]
    (when (<= 2 (count m))
      (let [[first-key second-key] (keys m)
            loc-data1 (edn-string-nav/get-path-in-clj-file (conj (vec (:in e)) first-key)
                                                           (:strictly-specking.core/file-source e))
            loc-data2 (edn-string-nav/get-path-in-clj-file (conj (vec (:in e)) second-key)
                                                           (:strictly-specking.core/file-source e))]
        (when (not= (:line loc-data1) (:line loc-data2))
          loc-data1)))))

(defn pprint-missing-keys-in-file-context [e]
  (let [m (get-in (::root-data e) (:in e))]
    (if-let [{:keys [line column value path loc]}
             (inline-missing-keys? e)]
      (do
        (println "File:" (str (::file-source e)))
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
   (and (::file-source e) (pprint-missing-keys-in-file-context e))
   (pprint-missing-keys-context e)))
