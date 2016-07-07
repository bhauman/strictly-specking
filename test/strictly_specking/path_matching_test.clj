(ns strictly-specking.path-matching-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [strictly-specking.path-matching :refer :all]
            [strictly-specking.parse-spec :as parse]))

#_(remove-ns 'strictly-specking.path-matching-test)

(load-file "dev-resources/test_specs/cljs_options_schema.clj")
(load-file "dev-resources/test_specs/test_schema.clj")
(in-ns 'strictly-specking.path-matching-test)

(alias 'c 'strictly-specking.cljs-options-schema)
(alias 't 'strictly-specking.test-schema)


(def mp-data
  {:cljsbuild
   {:builds
    {:dev
     {:source-paths ["src"]
      ;; :figwheel true ;; added in mpn-data
      :compiler
      {:figwheel
       {:websocket-host "localhost"
        :on-jsload      'example.core/fig-reload
        :on-message     'example.core/on-message
        :source-map true
        :debug true}}}}}})

(def vp-data
  {:cljsbuild
   {:builds
    [{:source-paths ["src"]
      ;; :figwheel true ;; added in vpn-data
      :compiler
      {:figwheel
       {:websocket-host "localhost"
        :on-jsload      'example.core/fig-reload
        :on-message     'example.core/on-message
        :source-map true
        :debug true}}}]}})

(def mpn-data (assoc-in mp-data [:cljsbuild :builds :dev :figwheel] true))
(def vpn-data (assoc-in vp-data [:cljsbuild :builds 0 :figwheel] true))

(def poss-paths
  (parse/find-key-path-without-ns ::t/lein-project-with-cljsbuild
                                  :figwheel))

(deftest best-possible-path-test
  (is (= (best-possible-path poss-paths mp-data)
         '({:into {},
            :matched :cljsbuild,
            :ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
            :ky :cljsbuild}
           {:into {}, :matched :builds,
            :ky-spec :strictly-specking.test-schema/builds,
            :ky :builds}
           {:ky :strictly-specking.core/pred-key,
            :ky-pred-desc :strictly-specking.test-schema/string-or-named,
            :into {},
            :matched :dev}
           {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})))

  (is (= (best-possible-path poss-paths mpn-data)
         '({:into {},
            :matched :cljsbuild,
            :ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
            :ky :cljsbuild}
           {:into {}, :matched :builds,
            :ky-spec :strictly-specking.test-schema/builds,
            :ky :builds}
           {:ky :strictly-specking.core/pred-key,
            :ky-pred-desc :strictly-specking.test-schema/string-or-named,
            :into {}} ;; <-- not matched
           {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})))
  
  (is (= (best-possible-path poss-paths vp-data)
         '({:into {},
            :matched :cljsbuild,
            :ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
            :ky :cljsbuild}
           {:into {},
            :matched :builds,
            :ky-spec :strictly-specking.test-schema/builds,
            :ky :builds}
           {:ky :strictly-specking.core/int-key, :into [], :matched 0}
           {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})))

    (is (= (best-possible-path poss-paths vpn-data)
         '({:into {},
            :matched :cljsbuild,
            :ky-spec :cljsbuild.lein-project.require-builds/cljsbuild,
            :ky :cljsbuild}
           {:into {},
            :matched :builds,
            :ky-spec :strictly-specking.test-schema/builds,
            :ky :builds}
           {:ky :strictly-specking.core/int-key, :into []} ;; <-- not matched
           {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})))

  )

(deftest generate-demo-data-test
  (is (= (generate-demo-data (best-possible-path poss-paths mp-data) :asdf)
         {:cljsbuild {:builds {:dev {:figwheel :asdf}}}}))
  (is (= (generate-demo-data (best-possible-path poss-paths mpn-data) :asdf)
         {:cljsbuild
          {:builds
           {[:key-like '(some-fn non-blank-string? keyword? symbol?)] {:figwheel :asdf}}}}))

  (is (= (generate-demo-data (best-possible-path poss-paths vp-data) :asdf)
         {:cljsbuild {:builds [{:figwheel :asdf}]}}))
  (is (= (generate-demo-data (best-possible-path poss-paths vpn-data) :asdf)
         {:cljsbuild {:builds [{:figwheel :asdf}]}}))
  (is (= (generate-demo-data (best-possible-path poss-paths vpn-data) :asdf)
         (generate-demo-data (best-possible-path poss-paths vp-data) :asdf)))


  )

