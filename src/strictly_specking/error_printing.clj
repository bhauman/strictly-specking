(ns strictly-specking.error-printing
  (:require
   [strictly-specking.ansi-util :as cl :refer [color]]
   [strictly-specking.annotated-pprint :as annot]
   [strictly-specking.edn-string-nav :as edn-string-nav]
   [strictly-specking.context-print :as cp]
   [clojure.string :as string]
   [clojure.pprint :as pp]
   [clojure.java.io :as io]))

#_(remove-ns 'strictly-specking.error-printing)

;; helpers from core
;; TODO move these to their own namespace or make functions in this ns
;; take the needed parameters

(defn error-in-path [e]
  (-> e :strictly-specking.core/error-path :in-path))

(defn error-path-parent
  "Given an error with an ::error-path return the path to the parent collection
of thie error element."
  [e]
  (-> e error-in-path butlast (or [])))

(defn error-key
  "Given an error with an ::error-path returh the key that this error is on."
  [e]
  (-> e error-in-path last))

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

(defn format-preds [types]
  (let [res (mapv type-lookup->str types)]
    (if (< (count (pr-str res)) 40)
      (string/join " | " res)
      (str "\n"
           (with-out-str
             (pp/with-pprint-dispatch pp/code-dispatch
               (pp/pprint (vec types))))))))

(defn format-predicate-str [{:keys [pred val]}]
  (cond
    (or
     (seq-with-first 'or pred)
     (seq-with-first 'some-fn pred))
    (str (if (every? *symbol-type-table* (rest pred))
           "It should be one of: "
           "It should satisfy one of: ")
         (format-preds (rest pred)))
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


(defmulti error-message :strictly-specking.core/error-type)

(defmethod error-message :default [e]
  (message-default-str e #_(select-keys e [:pred :path :val :reason :via :in])))

;; *** TODO: fill in the rest of the error types

(defmulti inline-message :strictly-specking.core/error-type)

(defmethod inline-message :default [e] (str "The key " (error-key e) " has a problem"))

#_(defmethod inline-message ::missing-required-keys [e]
  (str "XXXXX "
       (-> e :in last)
       " is missing"))

;; *** use pprint to print contextual errors

;; we could put a default value in the message-map
(defn add-missing-key [parent-path data ky]
  (let [parent-coll (get-in data parent-path)]
    (if (and
         (map? parent-coll)
         (not (contains? parent-coll ky)))
      (assoc-in data (conj (vec parent-path) ky) '...)
      data)))

(defn add-missing-keys [parent-path data kys]
  (reduce (partial add-missing-key parent-path) data kys))

(defn pprint-sparse-path
  ([data path key-message-map colors]
   (->>
    (pp/with-pprint-dispatch annot/error-path-dispatch
     (pp/pprint
      (annot/annotate-path-only
       (add-missing-keys path data (keys key-message-map))
       path
       {:abbrev true
        :comments (into {}
                        (map (fn [[k message]]
                               [k
                                (merge {:key-colors     [:error-key]
                                        :value-colors   [:none]
                                        :comment-colors [:pointer]
                                        ;; TODO parameterize what to fill in
                                        ;; only when needed
                                        ;; could be cool to show a generated sample value
                                        ;;:default-missing-value '_____
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

(defn file-line [file line column]
  (str (let [f (io/file file)]
         (if (.exists f)
           (.getAbsolutePath (io/file file))
           f))
       (if line (str ":" line) "") (if (and line column) (str ":" column) "")))

(defn pprint-in-file [file base-path key-message-map]
  (let [[k message] (first key-message-map) 
        path        (conj (vec base-path) k)]
    (when-let [{:keys [line column value path loc]}
               (edn-string-nav/get-path-in-clj-file path file)]
      (println (file-line file line column))
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


;; TODO should move this up to strictly specking core
;; *** pprint inline message
;;
;; this should dispatch and display in file context if there is file
;; information on the error

(defmulti pprint-inline-message :strictly-specking.core/error-type)

;; *** inserting missing keys as rows in an edn-string
;; in order to display missing keys in a file

;; todo this can be pushed down into the 

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
  (let [parent-path (error-path-parent e)
        m (get-in (:strictly-specking.core/root-data e)
                  parent-path)]
    (when (<= 2 (count m))
      (let [[first-key second-key] (keys m)
            loc-data1 (edn-string-nav/get-path-in-clj-file (conj (vec parent-path) first-key)
                                                           (:strictly-specking.core/file-source e))
            loc-data2 (edn-string-nav/get-path-in-clj-file (conj (vec parent-path) second-key)
                                                           (:strictly-specking.core/file-source e))]
        (when (not= (:line loc-data1) (:line loc-data2))
          loc-data1)))))

(defn pprint-missing-keys-in-file-context [e]
  (if-let [{:keys [line column value path loc]}
           (inline-missing-keys? e)]
    (do
      (println (file-line (:strictly-specking.core/file-source e) line column))
      (-> (cp/fetch-lines (:strictly-specking.core/file-source e))
          (cp/number-lines)
          (cp/insert-message line column
                             (str "Map is missing required key"
                                  (if (= 1 (count (:strictly-specking.core/missing-keys e)))
                                      "" "s")))
            (insert-missing-keys (:strictly-specking.core/missing-keys e) line column)
            (cp/extract-range-from-center line 10)
            cp/trim-blank-lines
            cp/blank-space-trim
            cp/format-line-numbers
            cp/color-lines
            cp/print-formatted-lines)
        true)
      (pprint-in-file (:strictly-specking.core/file-source e)
                      (error-path-parent e)
                      {(error-key e)
                       (str "Map is missing required key"
                            (if (= 1 (count (:strictly-specking.core/missing-keys e)))
                              (str ": " (pr-str (first (:strictly-specking.core/missing-keys e))))
                              (str "s: " (string/join ", "
                                                      (map pr-str
                                                           (:strictly-specking.core/missing-keys e))))
                              ))})))


