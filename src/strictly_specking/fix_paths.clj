(ns strictly-specking.fix-paths
  (:require [clojure.spec :as s]))

(declare fix-path)

(defn fix-path-true [p data path target-val]
  (when-let [res (fix-path data path target-val)]
    (cons p res)))

(defn handle-map-path [data path target-val]
  (let [p (first path)]
    (or (and
         (contains? data p)
         (or (fix-path-true p (get data p) (rest path) target-val)
             ;; more work for the val
             (and
              (#{0 1} (second path))
              (let [vv (if (= 0 (second path)) p (get data p))]
                (fix-path-true p vv (drop 2 path) target-val)))))
        ;; if it's a number look the tuple and call rest rest
        (and
         (number? p)
         (> (count data) p) ;; key exists
         (#{0 1} (second path))
         (let [[k v] (nth (seq data) p)
               vv (if (= 0 (second path)) k v)]
           (fix-path-true k vv (drop 2 path) target-val))))))

(defn handle-seq-path [data path target-val]
  (let [p (first path)]
    (and (number? p)
         (> (count data) p) 
         (fix-path-true p (nth data p) (rest path) target-val))))

(defn fix-path [data path target-val]
  (cond
    (and (= '() path) (= data target-val))
    '()
    (= '() path)
    false
    (map? data)
    (handle-map-path data path target-val)
    (coll? data)
    (handle-seq-path data path target-val)
    :else false))

;; there may be tiny ambiguous edge cases
;; {0 [0]} 
;; {:asdf 1
;;  1 [1]}
(defn fix-error-path*
  "searches for the correct path to the val in the current datastructure"
  [{:keys [in val]} data]
  (fix-path data in val))

(defn fix-error-path [err]
  (if-let [pth (fix-error-path* err (:strictly-specking.core/root-data err))]
    pth
    (:in err)))


(comment
  ;; these cases are ambiguous
  (s/explain-data (s/map-of keyword? ::s/any)
                  {0 [0]})
  
  (fix-error-path* {:path [0], :pred keyword?, :val 0, :via [], :in [0 0]} {0 [0]})
  
  (s/explain-data (s/map-of keyword? ::s/any)
                  {:asdf 1
                   1 [1]})

  (fix-error-path* {:path [0], :pred keyword?, :val 1, :via [], :in [1 0]}
                  {:asdf 1
                   1 [1]})

;; these work
  
  (s/explain-data (s/every (s/tuple keyword? (s/every (s/tuple keyword? integer?))))
                  {:asdf {:asdf ""}})

  (fix-error-path* {:path [1 0], :pred keyword?, :val 3, :via [], :in [0 1 0 0]}
                  {:asdf {3 1}})

  (fix-error-path* {:path [1 1], :pred integer?, :val "", :via [], :in [0 1 0 1]}
                  {:asdf {:asdf ""}})

  (s/explain-data (s/map-of keyword? (s/map-of keyword? integer?))
                  {:asdf {3 1}})

  (fix-error-path* {:path [1 0], :pred keyword?, :val 3, :via [], :in [:asdf 1 3 0]}
                  {:asdf {3 1}})

  (s/explain-data (s/map-of keyword? (s/map-of keyword? integer?))
                  {:asdf {:asdf ""}})

  (fix-error-path* {:path [1 1], :pred integer?, :val "", :via [], :in [:asdf 1 :asdf 1]}
                   {:asdf {:asdf ""}}))
