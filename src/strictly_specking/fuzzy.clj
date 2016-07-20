(ns strictly-specking.fuzzy)

(defn- next-row
  [previous current other-seq]
  (reduce
    (fn [row [diagonal above other]]
      (let [update-val (if (= other current)
                          diagonal
                          (inc (min diagonal above (peek row))))]
        (conj row update-val)))
    [(inc (first previous))]
    (map vector previous (next previous) other-seq)))

(defn levenshtein
  "Compute the levenshtein distance between two [sequences]."
  [sequence1 sequence2]
  (peek
    (reduce (fn [previous current] (next-row previous current sequence2))
            (map #(identity %2) (cons nil sequence2) (range))
            sequence1)))

;; this is a simple step function to determine the threshold
;; no need to figure out the numeric function
(defn length->thresh [k1 k2]
  (let [len (apply min (map (comp count name) [k1 k2]))]
    (cond
      ;; less than 4 characters no matching
      (<= len 4) 0
      (<= len 5) 1
      (<= len 6) 2
      (<= len 11) 3
      (<= len 20) 4
      :else (int (* 0.2 len)))))

(defn similar-key* [thresh ky ky2]
  {:pre [(keyword? ky) (keyword? ky2)]}
  (let [dist (levenshtein (str ky) (str ky2))]
    (when (<= dist thresh)
      dist)))

(defn similar-key [ky ky2]
  (and (keyword? ky)
       (keyword? ky2)
       (similar-key* (length->thresh ky ky2) ky ky2)))

(defn get-keylike [ky mp]
  (if-let [val (get mp ky)]
    [ky val]
    (when-let [res (not-empty
                    (sort-by
                     #(-> % first -)
                     (filter
                      #(first %)
                      (map (fn [[k v]] [(similar-key k ky)
                                        [k v]]) mp))))]
      (-> res first second))))

(defn fuzzy-select-keys [m kys]
  (into {} (keep #(get-keylike % m) kys)))

(defn fuzzy-select-keys-and-fix [m kys]
  (into {} (keep #(let [[_ v] (get-keylike % m)] [% v]) kys)))

