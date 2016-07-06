(ns strictly-specking.fix-paths-test
  (:require [clojure.spec :as s]
            [clojure.test :refer :all]
            [strictly-specking.fix-paths :refer [fix-error-path]]))


(defn test-e [sp d]
  (-> (s/explain-data sp d) ::s/problems first))

(defn fix-e [sp d]
  (fix-error-path (test-e sp d) d))

(deftest failing-base-cases
  ;; here are some bbase cases that fail
  ;; unless we can get more info

  ;; if these start passing it probably means that
  ;; the explain path has changed
  (is (not= (fix-e (s/map-of keyword? ::s/any) {0 [0]})
            [0]))
  (is (not= (fix-e (s/map-of keyword? ::s/any)
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
