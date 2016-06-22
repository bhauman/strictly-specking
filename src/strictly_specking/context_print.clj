(ns strictly-specking.context-print
  (:require
   [clojure.java.io :as io]
   [clojure.zip :as zip]
   [clojure.string :as string]
   [net.cgrand.sjacket :as sj]
   [net.cgrand.sjacket.parser :as p]))

(defn- at-newline? [loc]
  (= :newline (:tag (zip/node loc))))

(defn line-number-of [root-zip l]
  (let [node (zip/node l)]
    (loop [c 1
           loc root-zip]
      (if (= (zip/node loc) node)
        c
        (recur
         (if (at-newline? loc) (inc c) c)
         (zip/next loc))))))

(def line-number (partial #'line-number-of z))

(defn node-test [tag n]
  (and (map? n)
       (= (:tag n) tag)))

(def keyword-node? (partial node-test :keyword))
(def map-node?     (partial node-test :map))
(def set-node?     (partial node-test :set))
(def list-node?    (partial node-test :list))
(def vector-node?  (partial node-test :vector))
(def coll-node?    (some-fn map-node? list-node? vector-node?))
(def name-node?    (partial node-test :name))
(def symbol-node?  (partial node-test :symbol))

(defn is-keyword? [k n]
  (and (keyword-node? n)
       (= (get-in n [:content 1 :content])
          [(name k)])))

(defn is-project? [n]
  (and (name-node? n)
       (= (:content n)
          ["defproject"])))

(defn node [loc]
  (and loc
       (zip/node loc)))

#_(is-keyword? :description
             {:tag :keyword, :content [":" {:tag :name, :content ["description"]}]})

(defn- insignificant? [loc]
  (when-let [{:keys [tag]} (zip/node loc)]
    (p/space-nodes tag)))

(defn- find-first [pred loc]
  (->> loc
       (iterate zip/next)
       (take-while (comp not zip/end?))
       (filter (comp pred zip/node))
       first))

(defn find-project [loc]
  (->> loc
       (find-first is-project?)
       zip/up))

(defn lazy-right-list [loc]
  (->> loc
       (iterate zip/right)
       (take-while (comp not nil?))
       (remove insignificant?)))

(defn find-key-in-structure [k loc]
  (->> loc
       lazy-right-list
       (drop 1)           
       (partition 2)
       (map first)
       (filter (comp (partial is-keyword? k) zip/node))
       first))

(defn- find-key-in-project [k loc]
  (->> loc
       find-project
       (find-key-in-structure k)))

(defn find-key-in-map [k loc]
  (assert (map-node? (zip/node loc)) "Must be a map node.")
  (->> loc
       zip/down
       (find-key-in-structure k)))

(defn find-key-in-seq [k loc]
  (assert (integer? k) "Key to seq must be integer.")
  (->> loc
       zip/down
       lazy-right-list
       (drop 1)
       (drop k)
       first))

(defn project-node? [n]
      (and (symbol-node? n)
           (->> n :content first is-project?)))

(defn- next-value [loc]
  (->> loc
       lazy-right-list
       (drop 1)
       first))

(defn find-key-in-node [k loc]
  (when-let [n (zip/node loc)]
    (cond
      (project-node? n) (find-key-in-project k loc)
      (map-node? n)     (find-key-in-map k loc)
      (vector-node? n)  (find-key-in-seq k loc)
      (list-node? n)    (find-key-in-seq k loc)
      ;; TODO sets
      :else nil)))

(defn find-key-value-in-node [k loc]
  (when-let [res (find-key-in-node k loc)]
    (let [n (zip/node loc)]
      (cond
        (project-node? n) (next-value res)
        (map-node? n)     (next-value res)
        :else res))))

(defn get-value-at-path
  "given a path into a structure return its location"
  [path loc]
  (reduce #(find-key-value-in-node %2 %1) loc (butlast path)))

(defn get-loc-at-path
  "given a path into a structure return its location"
  [path loc]
  (when-let [value-above (get-value-at-path path loc)]
    (find-key-in-node (last path) value-above)))

(defn file-to-parsed-zipper [file]
  (when (.exists (io/file file))
    (-> (io/file file)
        slurp
        p/parser
        zip/xml-zip)))

(defn file-to-initial-position [file]
  (when-let [loc (file-to-parsed-zipper file)]
    (if (= "project.clj" (.getName (io/file file)))
      (find-project loc)
      (find-first coll-node? loc))))

(defn- sjacket->clj [value]
  (try
    (if-not (#{:comment :whitespace :newline} (:tag value))
      (-> value sj/str-pt read-string))
    (catch Throwable e
      nil)))

(defn get-path-in-clj-file
  "Given the name of a file that holds a collection of EDN data or
   a project.clj file. This function will traverse the given path and
   return a map of the following info:

     :line - the line number where the found item
     :column - the column number of the found item
     :value  - the value of the found item
     :path   - the path that was supplied as an argument
     :loc    - the zipper at this point

   There is some ambiguity around whether this represents a key and a
   value or just a value at a point.  If the last key is a member of a
   map, the value returned will be the value at the key position.
   Otherwise, the value at the position itself will be returned.
"

  [path file]
  (when-let [loc (file-to-initial-position file)]
    (when-let [point-loc (get-loc-at-path path loc)]
      {:line (line-number point-loc)
       :column (sj/column point-loc)
       :file file
       :value  (or (sjacket->clj (-> point-loc next-value zip/node))
                   (sjacket->clj (zip/node point-loc)))
       :path path
       ; :loc point-loc
       })))


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
  (let [lines   (string/split msg #"\n")
        prefix        (blanks column)
        prefix-indent (str prefix (blanks 6))]
    (->> (rest lines)
         (map #(str prefix-indent %))
         (string/join "\n")
         (str prefix "^---  " (first lines) "\n"))))

(defn insert-message [numbered-lines line column message]
  (concat
   (take-while #(not= (second %) line) numbered-lines)
   [[:error-line line (last (nth numbered-lines (dec line)))]]
   [[:message-line nil (format-message column message)]]
   (drop-while #(not= (second %)
                      (inc line)) numbered-lines)))

(defn extract-range [formatted-lines center half-range]
  (filter
   #(let [[_ a _] %]
      (or (nil? a)
          (< (- center half-range) a (+ center half-range))))
   formatted-lines))

(defn max-line-number [formatted-lines]
  (-> formatted-lines
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
    (when (> min-lead 6)
      (let [subtract-lead (- min-lead 6)]
        (map (fn [[t l b]]
               [t l (if (string/blank? b)
                      b
                      (subs b subtract-lead))])
             formatted-lines)))))

(defn print-formatted-lines [formatted-lines]
  (let [max-char-length (+ 2 (count (str (max-line-number formatted-lines))))]
    (doseq [[t l b] formatted-lines]
      (condp = t
        :line
        (println (str (format (str "%" max-char-length "d ") l) b))
        :error-line
        (println (str (format (str "%" max-char-length "d ") l) b))        
        :message-line
        (doseq [b (string/split b #"\n")]
          (println (str (blanks (inc max-char-length))
                        b)))))))

#_(mapv println (map last
                   
                   (-> (fetch-lines "project.clj")
                       number-lines
                       (insert-message 35 28 "here I am \nonce again \nonce again  \nonce again again")
                       (extract-range 35 10)
                       blank-space-trim
                       print-formatted-lines
                       )

                   )
      )


#_(slurp (io/file "project.clj"))



#_(get-path-in-clj-file [:cljsbuild :builds 1 :figwheel :debug] "test.edn")


#_(find-project z)

#_(->> edn
       (find-first coll-node?)
       (get-value-at-path [:cljsbuild :builds])
       #_(find-key-value-in-node :cljsbuild)
       #_(find-key-value-in-node :builds)
       #_(find-key-value-in-node 0)
       #_(find-key-in-node :id)
       (get-loc-at-path [:cljsbuild :builds])
       
       #_(get-loc-at-path [:cljsbuild :builds ])
       )

#_(-> z
     find-project
     (get-loc-at-path [:cljsbuild :builds 1 :id])
    
    ((juxt 
      line-number
      sj/column)))


