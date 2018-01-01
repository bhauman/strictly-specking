(ns strictly-specking.fix-paths-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [strictly-specking.fix-paths :as sfp :refer [fix-err* fix-error-path]]))


(defn test-e [sp d]
  (-> (s/explain-data sp d) ::s/problems first))

(defn fix-e [sp d]
  (fix-err* (test-e sp d) d))

(deftest failing-base-cases
  ;; here are some tough base cases that where fixed

  (is (= (fix-e (s/map-of keyword? any?) {0 [0]})
            [0]))
  (is (= (fix-e (s/map-of keyword? any?)
                   {:asdf 1
                    1 [1]})
            [1])))

(s/def ::badly (s/every (s/tuple keyword? (s/map-of keyword? integer?))))

(s/def ::madly (s/every ::badly))

;; test-check would be more fun here

(deftest normal-cases
  (is (= (fix-e (s/every (s/tuple keyword? (s/every (s/tuple keyword? integer?))))
                {:asdf {3 1}})
         '(:asdf 3)))

  (is (= (fix-e (s/every (s/tuple keyword? (s/every (s/tuple keyword? integer?))))
                {:asdf {:asfd ""}})
         '(:asdf :asfd)))

  (is (= (fix-e (s/map-of keyword? (s/map-of keyword? integer?))
                {:asdf {3 1}})
         '(:asdf 3)))
  (is (= (fix-e (s/map-of keyword? (s/map-of keyword? integer?))
                {:asdf {:asfd ""}})
         '(:asdf :asfd)))
  
  (is (= (fix-e (s/map-of keyword? (s/keys :opt-un [::badly]))
                {:howdy {::badly {:keysly {:corner "asdf"}}}})
         '(:howdy :strictly-specking.fix-paths-test/badly :keysly :corner)))
  
  (is (= (fix-e (s/map-of keyword? (s/keys :opt-un [::badly]))
                {:howdy {::badly {:keysly "asdf"}}})
         '(:howdy :strictly-specking.fix-paths-test/badly :keysly)))

  (is (= (fix-e (s/map-of keyword? (s/keys :opt-un [::badly]))
                {:howdy {::madly [{:keysly {:howdy 1}}
                                  {:keysly "asdf"}]}})
         '(:howdy :strictly-specking.fix-paths-test/madly 1 :keysly)))  
  )

(s/def ::darling (s/map-of keyword? (s/every (s/tuple integer? (s/every integer?)))))

(s/def ::rarling (s/map-of keyword? (s/map-of keyword? (s/every integer?))))

(s/def ::marlin (s/map-of keyword? (s/map-of keyword? (s/every (s/and
                                                                (s/or :in integer?
                                                                      :str string?)
                                                                map?)))))

(s/def ::starlin (s/map-of keyword?
                           (s/map-of integer? (s/every integer?) #_(s/every (s/and
                                                                            (s/or :in integer?
                                                                                  :str string?)
                                                                            map?)))))



(deftest harder-cases
  (is (= (fix-e
          (s/map-of keyword? (s/every ::darling))
          {:hello [{:hello {2 [55 66]
                            1 [66 77]
                            0 ["asdf" "asdf"]}}]})
         '(:hello 0 :hello 0 0)))

  (is (= (fix-e
          (s/map-of keyword? (s/every ::rarling))
          {:hello [{:hello {:hello ["asdf" "Asdff"]}}]})
         '(:hello 0 :hello :hello 0)))

  ;; failing because of acting on a conform
  (is (= (fix-e
          (s/map-of keyword? (s/every ::marlin))
          {:hello [{:hello {:hello ["asdf" "Asdff"]}}]})
         '(:hello 0 :hello :hello 0 ::sfp/search-failure)))

  
  (is (= (fix-e
          (s/map-of keyword? (s/every ::starlin))
          {:hello [{:hello {0 ["asdf" "asd"]}}]})
         '(:hello 0 :hello 0 0)))

  )


(s/def ::asdf
  (s/keys :req-un [::asdf1]))

(s/def ::asdf1
  (s/keys :req-un [::asdf2]))

(s/def ::asdf2
  (s/keys :req-un [::asdf3]))

(s/def ::asdf3 (s/every (s/tuple keyword? (s/keys :req-un [::asdf4]))))

(s/def ::asdf4 integer?)

(deftest handle-keys-well
  (is (= (fix-e
          ::asdf
          {:asdf1 {:asdf2 {:asdf3 {:hello {:asdf4 "asdf"}}}}})
         '(:asdf1 :asdf2 :asdf3 :hello :asdf4)))
  )
