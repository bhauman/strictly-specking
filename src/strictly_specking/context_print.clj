(ns strictly-specking.context-print
  (:require
   [strictly-specking.ansi-util :refer [color]]
   [clojure.pprint :as pp]
   [clojure.java.io :as io]
   [clojure.string :as string]))

;; TODO make it general and possible to point to any structure on a path

;; * Displaying a message in context of file

(defn fetch-lines [file]
  (let [file (io/file file)]
    (when (.exists file)
      (doall (line-seq (io/reader file))))))

(defn number-lines [lines]
  (map-indexed #(vector :line (inc %1)
                        %2) lines))

(defn blanks [n]
  (apply str (repeat n " ")))

;; wrapping the message intellgently
(defn determine-message-format [column msg]
  (let [parts (string/split msg #"\n")
        message-width (reduce max (map count parts))
        marker-width 6
        max-width 80]
    ;; need to decide if it can go before the marker
    (cond
      (> (+ message-width marker-width) max-width)
      ;; message on newline at start
      (cons (format (str "%" column "s%s") "" "^---")
              (map
               #(format (str "%" message-width "s") %)
               parts))
      (> (+ message-width marker-width column) max-width)
      (if (< (+ message-width 2) column)
        ;; message before
        (cons (format (str "%" (- column 2) "s  %s") (first parts) "^---" )
              (map
               #(format (str "%" (- column 2) "s") % )
               (rest parts)))
        ;; message newline centered
        (cons (format (str "%" column "s%s") "" "^---")
              (map
               #(format (str "%" (int (/ (- max-width message-width) 2))
                             "s%s") "" %)
               parts)))
      :else
      ;; message after
      (cons (format (str "%" column "s%s  %s") "" "^---" (first parts))
            (map
             #(format (str "%" (+ column marker-width) "s%s") "" %)
             (rest parts))))))

(comment
  
  (determine-message-format 20
                            "asdf asdf asdf\nasdf asdf asdf asdf")

  (determine-message-format 60
                            "asdf asdf asdf\nasdf asdf asdf asdf")
  
  (determine-message-format
   40
   "Makes a function which can directly run format-in. The function is
fn [& args] ... and returns nil. This version of the formatter macro is
designed to be used with *out* set to an appropriate Writer. In particular,
this is meant to be used as part of a pretty printer dispatch method.

format-in can be either a control string or a previously compiled format.")
  
  (determine-message-format
   40
   "Makes a function which can directly run format-in.
fn [& args] ... and returns nil. This version of
designed to be used with *out* set to an appropriate Wri
this is meant to be used as part of a 

format-in can be either a control string or a p")
  )

(defn format-message [column msg]
  (mapv #(vector :message-line nil %)
        (determine-message-format column msg)))

(defn insert-message [numbered-lines line column message]
  (concat
   (take-while #(not= (second %) line) numbered-lines)
   [[:error-line line (last (nth numbered-lines (dec line)))]]
   (format-message column message)
   (drop-while #(not= (second %)
                      (inc line)) numbered-lines)))

(defn extract-range [formatted-lines start end]
  (filter
   #(let [[_ a _] %]
      (or (nil? a)
          (<= start a end)))
   formatted-lines))

(defn extract-range-from-center [formatted-lines center half-range]
  (extract-range formatted-lines (- center half-range) (+ center half-range)))

(defn trim-blank-lines* [lines]
  (drop-while #(string/blank? (nth % 2)) lines))

(defn trim-blank-lines [lines]
  (-> lines
      reverse
      trim-blank-lines*
      reverse
      trim-blank-lines*))

(defn max-line-number [formatted-lines]
  (->> formatted-lines
      (map second)
      (filter number?)
      (reduce max)))

(defn min-blank-lead [formatted-lines]
  (->> formatted-lines
       (map last)
       (filter (complement string/blank?))
       (map #(count (take-while #{\space} %)))
       (reduce min)))

(defn blank-space-trim [formatted-lines]
  (let [min-lead (min-blank-lead formatted-lines)]
    (if (> min-lead 6)
      (let [subtract-lead (- min-lead 6)]
        (map (fn [[t l b]]
               [t l (if (string/blank? b)
                      b
                      (subs b subtract-lead))])
             formatted-lines)))
    formatted-lines))

(defn format-line-numbers [formatted-lines]
  (let [max-char-length (+ 2 (count (str (max-line-number formatted-lines))))]
    (map
     (fn [[t l b]]
       [t
        (if (and (#{:line :error-line} t) (integer? l))
          (format (str "%" max-char-length "d ") l)
          (str (blanks (inc max-char-length))))
        b])
     formatted-lines)))

(def default-line-colors
  {:error-line   :bold
   :message-line :magenta
   :line-number  :cyan
   :line         nil})

(defn style-when [l colr]
  (if (and colr l)
    (color l colr)
    l))

(defn color-lines
  ([formatted-lines]
   (color-lines formatted-lines {}))
  ([formatted-lines colors]
   (let [{:keys [line line-number message-line error-line]}
         (merge default-line-colors colors)]
     (mapv
      (fn [[t l b]]
        [t (style-when l line-number)
         (condp = t
           :line
           (style-when b line)
           :error-line
           (style-when b error-line)
           :message-line
           (style-when b message-line)
           b)])
      formatted-lines))))

(defn ->string [formatted-lines]
  (string/join "\n"
               (map (fn [[t l b]]
                      (str l b))
                    formatted-lines)))

(defn print-formatted-lines [formatted-lines]
  (doseq [[t l b] formatted-lines]
    (println (str l b))))

(defn print-message-in-context-of-file
  "Given a line number, a column number and a message. This function
  will print out the message in the context of the file"
  [file line column message]
  (-> (fetch-lines file)
      number-lines
      (insert-message line column message)
      (extract-range-from-center line 5)
      trim-blank-lines
      blank-space-trim
      format-line-numbers
      color-lines
      print-formatted-lines
      ))

#_(print-message-in-context-of-file "project.clj" 35 28 "here I am" )

#_(-> (fetch-lines "project.clj")
      number-lines
      (insert-message 35 28 "here I am \nonce again \nonce again  \nonce again again")
      (insert-missing-keys [:asdf :ASDFG] 35 28)
      (extract-range-from-center 35 10)
      blank-space-trim
      format-line-numbers
      #_->string
      color-lines
      print-formatted-lines
      )

