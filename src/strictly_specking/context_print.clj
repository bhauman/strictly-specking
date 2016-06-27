(ns strictly-specking.context-print
  (:require
   [clansi.core :as ansi]
   [clojure.java.io :as io]
   [clojure.string :as string]))

;; TODO make it general and possible to point to any structure on a path

;; displaying error in context of file

(defn fetch-lines [file]
  (let [file (io/file file)]
    (when (.exists file)
      (doall (line-seq (io/reader file))))))

(defn number-lines [lines]
  (map-indexed #(vector :line (inc %1)
                        %2) lines))

(defn blanks [n]
  (apply str (repeat n " ")))

(defn format-message [column msg]
  (when-let [lines   (not-empty (string/split msg #"\n"))]
    (->> (rest lines)
         (map #(str (blanks (+ 6 column)) %))
         (cons (str (blanks column) "^---  " (first lines) "\n"))
         (mapv #(vector :message-line nil %)))))

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
  {:error-line   :bright
   :message-line :magenta
   :line-number  :cyan
   :line         nil})

(defn style-when [l color]
  (if (and color l)
    (ansi/style l color)
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
      (extract-range-from-center line 10)
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

