(ns strictly-specking.core-test
  (:require [clojure.test :refer :all]
            [strictly-specking.core :refer :all :as sp]
            [strictly-specking.parse-spec :as parse]
            [clojure.spec :as s]))

(s/def ::id (s/or :string string? :keyword keyword?))
(s/def ::source-paths (s/+ string?))
(s/def ::assert #(or (true? %) (false? %)))
(s/def ::build-config (strict-keys :req-un [
                                            ::id
                                            ::source-paths]
                                   :opt-un [::assert]))
    
(s/def ::http-server-root string?)
(s/def ::server-port      integer?)
(s/def ::server-ip        string?)
(s/def ::builds           (s/* ::build-config))
    
(s/def ::figwheel (strict-keys
                   :opt-un [::http-server-root
                            ::server-port
                            ::server-ip]
                   :req-un [::builds]))

(s/def ::dimensions (s/tuple integer? integer?))

(s/def ::bedroom
  (strict-keys :opt-un [::door
                        ::dimensions]))

(s/def ::attic (strict-keys
                :opt-un [::bedroom
                         ::bathroom]))

(s/def ::windows integer?)
(s/def ::door boolean?)
(s/def ::name string?)

(s/def ::house (strict-keys
                :opt-un [::windows
                         ::attic
                         ::bedroom
                         ::bathroom]
                :req-un [::name 
                         ::door]))

(s/def ::houses (s/+ ::house))

(s/def ::cljsbuild
  (strict-keys
   :req-un [::houses]))

(s/def ::root (strict-keys
               :opt-un [::cljsbuild
                        ::figwheel]))

#_(s/explain-data (strict-keys :opt-un [::attic])
                {:attic 1})

(defn problem-map [x]
  (into {}
        (map (juxt :path identity)
             (::s/problems x))))



(deftest misspell
  (let [e (problem-map (s/explain-data ::build-config {:idd "asfd"
                                                        :source-path ["src"]}))]
    (is (get e [:misspelled-key :idd]))
    (is (get e [:misspelled-key :source-path]))
    (is (-> e
            (get [:misspelled-key :idd])
            ::sp/misspelled-key
            (= :idd)))
    (is (-> e
            (get [:misspelled-key :idd])
            ::sp/correct-key
            (= :id)))

    (is (-> e
            (get [:misspelled-key :source-path])
            ::sp/misspelled-key
            (= :source-path)))
    (is (-> e
            (get [:misspelled-key :source-path])
            ::sp/correct-key
            (= :source-paths)))
    )

  (let [e (problem-map
           (s/explain-data ::root {:cljsbuild "asfd"
                                   :figwheel  {:builds
                                               [{:idd "asfd"
                                                 :source-path ["src"]}]}}))]
    (is (get e [:cljsbuild]))
    (is (-> e
            (get [:cljsbuild])
            :pred
            (= 'map?)))
    
    (is (get e  [:figwheel :builds :misspelled-key :idd]))
    (is (get e  [:figwheel :builds :misspelled-key :source-path]))
    (is (-> e
            (get [:figwheel :builds :misspelled-key :idd])
            ::sp/misspelled-key
            (= :idd)))
    (is (-> e
            (get [:figwheel :builds :misspelled-key :idd])
            ::sp/correct-key
            (= :id)))

    (is (-> e
            (get [:figwheel :builds :misspelled-key :source-path])
            ::sp/misspelled-key
            (= :source-path)))
    (is (-> e
            (get [:figwheel :builds :misspelled-key :source-path])
            ::sp/correct-key
            (= :source-paths)))
    
    ))

(deftest replacement
  (let [e (problem-map
           (s/explain-data ::root {:cljsbuild "asfd"
                                   :asdf  {:builds
                                           [{:idd "asfd"
                                             :source-path ["src"]}]}}))]
    (is (e [:wrong-key :asdf]))
    (is (-> e
            (get [:wrong-key :asdf])
            ::sp/wrong-key
            (= :asdf)))
    (is (-> e
            (get [:wrong-key :asdf])
            ::sp/correct-key
            (= :figwheel)))
    
    )
  )

(s/describe (s/merge
             (s/keys :opt-un [::house])
             (s/keys :opt-un [::house])))

(deftest describe-expections-for-path-finding
  (is (= (s/describe (s/keys :opt-un [::a ::b]
                             :opt [::c ::d]))
         '(keys
           :opt
           [::c ::d]
           :opt-un
           [::a ::b])))

  (is (= (s/describe (strict-keys :opt-un [::a ::b]
                                  :opt [::c ::d]))
         '(strict-keys
           :opt
           [::c ::d]
           :opt-un
           [::a ::b])))

  (is  (parse/expanded-map-of-desc? (s/describe (s/map-of keyword? even?))))

  (is (= (s/describe (s/or :yep (s/map-of keyword? even?)))
         '(or :yep (map-of keyword? even?))))

  (is (= (s/describe (s/and (s/map-of keyword? even?)))
         '(and (map-of keyword? even?))))

  (is (= (take 2 (s/describe (s/coll-of keyword?)))
         '(every keyword?)))

  (is (= (s/describe (s/and (s/coll-of keyword?)))
         '(and (coll-of keyword?))))

  (is (= (s/describe (s/+ even?))
         '(+ even?)))
  (is (= (s/describe (s/* even?))
         '(* even?)))
  (is (= (s/describe (s/? even?))
         '(? even?)))

  )

;; TODO more pathfinding and parsing tests
(deftest pathfinding
  (is (= (parse/find-key-path ::root ::bedroom)
         #{(list {:ky-spec ::cljsbuild, :ky :cljsbuild}
                 {:ky-spec ::houses, :ky :houses}
                 {:ky :strictly-specking.core/int-key}
                 {:ky-spec ::bedroom, :ky :bedroom})
           (list {:ky-spec ::cljsbuild, :ky :cljsbuild}
                 {:ky-spec ::houses, :ky :houses}
                 {:ky :strictly-specking.core/int-key}                 
                 {:ky-spec ::attic, :ky :attic}
                 {:ky-spec ::bedroom, :ky :bedroom})}))
  )
