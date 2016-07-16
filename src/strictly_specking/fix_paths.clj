(ns strictly-specking.fix-paths)

#_(remove-ns 'strictly-specking.fix-paths)

(defn failed-path-search [result-path]
  (= (last result-path) ::search-failure))

(defn valid-int-path? [path-elem data]
  (and (integer? path-elem) (< path-elem (count data))))

(defn first-or-longest-success [paths]
  (or (first (filter (complement failed-path-search) paths))
      (first (sort-by (comp - count) paths))))

(declare fp)

(defn flip-integer-key-ref [err [path-elem :as path] data]
  (let [k (first (nth (seq data) path-elem))]
        (fp err (cons k (rest path)) data)))

(defn fp [err [path-elem :as path] data]
  (cond
    (empty? path)
    (if (and (::check-val err) (<= (::check-val err) 3))
      (if (= (:val err) data) [] [::search-failure])
      [])
    
    (and (map? data)
         (contains? data path-elem))
    (let [handle-map-fn
          (fn [tuple-data value-data]
            (let [res (lazy-seq [(fp err (rest path) value-data)])]
              (if (#{0 1} (second path))
                (first-or-longest-success
                 (cons (rest (fp (assoc err ::check-val 1) (rest path) tuple-data))
                       res))
                (first res))))]
      (first-or-longest-success
       (cons (cons path-elem
                   (handle-map-fn (find data path-elem) (get data path-elem)))
             ;; rare case: we have an int path-elem and a that is contained in the map
             ;; but was actually intended to be a seq like location
             (lazy-seq
              (when (valid-int-path? path-elem data)
                (let [[k v :as map-ent] (nth (seq data) path-elem)]
                  [(cons k (handle-map-fn map-ent v))]))))))
    
    (and (map? data) (valid-int-path? path-elem data))
    (flip-integer-key-ref err path data)
    
    (and (sequential? data) (valid-int-path? path-elem data))
    (cons path-elem (fp (if (::check-val err)
                          (update-in err [::check-val] inc)
                          err)
                        (rest path) (nth (seq data) path-elem)))

    :else
    [::search-failure]))

(defn fix-error-path
  "searches for the correct path to the val in the current datastructure"
  [{:keys [in] :as err} data]
  (let [res (fp err in data)]
    (when-not (failed-path-search res)
      res)))

