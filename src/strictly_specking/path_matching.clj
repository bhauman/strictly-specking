(ns strictly-specking.path-matching
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [clojure.pprint :as pp]
   [strictly-specking.strict-keys :as strict-impl]))

#_(remove-ns 'strictly-specking.path-matching)

;; 1. the key is recognized among the global keys and the value fuzzy conforms

;; or the key is recognized as a misspelled global key and the value fuzzy conforms

;; 2. we find a likely path to the recognized key

;; does this path already exist?
;; TODO does the :val fuzzy-conform?
(defn filter-possbile-path-choices
  "Given a list or paths that have the form returned by parse-spec/find-key-paths

Filter out the paths that don't make any sense based on the value that the
focused key currently has.

Basically checks to see if the proposed path ends with a key spec that
fails against the current value (or doesn't even fuzzy conform.
"
  [key-val possible-paths]
  ;; first is the value valid for this key-spec?
  (if-let [paths
           (not-empty
            (filter (fn [x]
                      (s/valid? (-> x last :ky-spec)
                                key-val))
                    possible-paths))]
    paths
    ;; if not is this a map and does it fuzzy conform
    (if-let [score-paths
             (not-empty
              (keep (fn [x]
                      (when (map? key-val)
                        [(strict-impl/fuzzy-conform-score-spec-key
                          (-> x last :ky-spec)
                          key-val)
                         x]))
                    possible-paths))]
      (->> score-paths
           (filter #(> (first %) 0.5))
           (sort-by (comp - first))
           ;; dropping the score here is probably a bad idea
           ;; as we are ranking the paths again later
           (map second))
      [])))

;; when there is more than one path let's look at the existing
;; structure to see if we can match it

(defn decorate-type [data possible-path-elem]
  (if (coll? data)
    (assoc possible-path-elem
           :into
           (cond
             (map? data)        {}
             (vector? data)     []
             (set? data)        #{}
             (sequential? data) '())
           ;; should just add actual data
           ;; :data data
           )
    possible-path-elem))

(declare path-match)

(defn path-match-helper [data possible-path kv-list]
  (->> kv-list
      (mapcat
       #(map (partial cons (assoc (decorate-type data (first possible-path))
                                  :matched (first %)))
             (path-match (rest possible-path) (second %))))
      distinct))

(defn path-match [possible-path data]
  (let [p (first possible-path)
        match-next (partial path-match-helper data possible-path)]
    (if (or (nil? data) (nil? p))
      [[]]
      (or
       (condp = (:ky p)
         :strictly-specking.core/int-key
         (when (sequential? data)
           (concat [[(decorate-type data p)]]
                   (match-next (map-indexed vector data))))
         :strictly-specking.core/pred-key
         (when (and (map? data)
                    (not= (:ky-pred-desc p) any?))
           (concat [[(decorate-type data p)]]
                   (match-next data)))
         (and (map? data) (contains? data (:ky p))
              (match-next (select-keys data [(:ky p)]))))
       [[]]))))

;; really need to test the above in different situations

(def wildcard-key?
  #{:strictly-specking.core/int-key
    :strictly-specking.core/pred-key})

(defn score-path [p possible-path]
  (let [wilds (* 1/4 (count (filter (comp wildcard-key? (some-fn :matched :ky))
                                    p)))]
    (/ (- (count p) wilds)
       (count possible-path))))

(defn rank-path-matches [possible-path data]
  (->> (path-match possible-path data)
       (filter #(not= (-> % last :ky)
                      (-> possible-path last :ky)))
       (map (fn [p] [(score-path p possible-path)
                     p
                     possible-path]))
       (sort-by (comp - first))))

(defn path-matches [possible-paths data]
  (->> possible-paths
       (mapcat
        #(rank-path-matches % data))))

(defn fill-out-path [partial-path possible-path]
  (concat
   partial-path
   (drop (count partial-path) possible-path)))

(defn best-possible-path [possible-paths data]
  (when-let [[part poss]
             (->> (path-matches possible-paths data)
                  (sort-by (comp - first))
                  first
                  rest
                  not-empty)]
    (fill-out-path part poss)))

(defn gen-into-data [p]
  (or (:into p)
      (condp = (:ky p)
        :strictly-specking.core/pred-key {}
        :strictly-specking.core/int-key []
        {})))

(defn gen-key [p]
  (condp = (:ky p)
    :strictly-specking.core/pred-key
    (or (:matched p)
        [:key-like (s/describe (:ky-pred-desc p))])
    (:ky p)))

(defn generate-demo-data [suggested-path target-val]
  (reduce
   (fn [d p]
     (let [ind (gen-into-data p)]
       (if (map? ind)
         (into ind [[(gen-key p) d]])
         (into ind [d]))))
   target-val
   (reverse suggested-path)))

(defn generate-demo-path [suggested-path]
  (map
   #(if (= :strictly-specking.core/int-key (:ky %))
      0
      (gen-key %))
   suggested-path))
