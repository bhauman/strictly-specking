(ns specly.core-test
  (:require [clojure.test :refer :all]
            [specly.core :refer :all :as sp]
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


(deftest misspell
  (let [e (::s/problems
           (s/explain-data ::build-config {:idd "asfd"
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

  (let [e (::s/problems
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
  (let [e (::s/problems
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

(deftest describe-expections-for-path-finding
  (is (= (s/describe (s/keys :opt-un [::a ::b]
                             :opt [::c ::d]))
         '(keys
           :opt
           [:specly.core-test/c :specly.core-test/d]
           :opt-un
           [:specly.core-test/a :specly.core-test/b])))

  (is (= (s/describe (strict-keys :opt-un [::a ::b]
                                  :opt [::c ::d]))
         '(strict-keys
           :opt
           [:specly.core-test/c :specly.core-test/d]
           :opt-un
           [:specly.core-test/a :specly.core-test/b])))

  (is (= (s/describe (s/map-of keyword? even?))
         '(and (coll-of (tuple keyword? even?) {}) map?)))

  (is (= (s/describe (s/or :yep (s/map-of keyword? even?)))
         '(or :yep (map-of keyword? even?))))

  (is (= (s/describe (s/and (s/map-of keyword? even?)))
         '(and (map-of keyword? even?))))

  (is (= (s/describe (s/coll-of keyword? []))
         '(coll-checker keyword?)))

  (is (= (s/describe (s/and (s/coll-of keyword? [])))
         '(and (coll-of keyword? []))))

  (is (= (s/describe (s/+ even?))
         '(+ even?)))
  (is (= (s/describe (s/* even?))
         '(* even?)))
  (is (= (s/describe (s/? even?))
         '(? even?)))

  )

(deftest pathfinding
  (is (= (sp/find-key-path ::root ::bedroom)
         #{(list {:ky-spec :specly.core-test/cljsbuild, :ky :cljsbuild}
                 {:ky-spec :specly.core-test/houses, :ky :houses}
                 {:ky :specly.core/int-key}
                 {:ky-spec :specly.core-test/bedroom, :ky :bedroom})
           (list {:ky-spec :specly.core-test/cljsbuild, :ky :cljsbuild}
                 {:ky-spec :specly.core-test/houses, :ky :houses}
                 {:ky :specly.core/int-key}
                 {:ky-spec :specly.core-test/attic, :ky :attic}
                 {:ky-spec :specly.core-test/bedroom, :ky :bedroom})}))
  )




