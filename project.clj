(defproject strictly-specking "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha8"]
                 #_[org.clojure/clojure "1.6.0"]
                 [net.cgrand/sjacket  "0.1.1"
                  :exclusions [org.clojure/clojure]]
                 [myguidingstar/clansi "1.3.0"
                  :exclusions [org.clojure/clojure]]]

  :profiles {:dev {:source-paths ["dev" "src"]
                   :aliases
                   {"standalone" ["run" "-m" "strictly-specking-tasks.core" ":standalone"]
                    "with-spec" ["run" "-m" "strictly-specking-tasks.core" ":with-clojure-spec"]}}}
  
  ;; just to test project parsing
  :cljsbuild {:assert true
              :builds [{ :id "example-admin"
                        :source-paths ["src" "dev" "tests" "../support/src"]
                        :notify-command ["notify"]
                        :figwheel
                        { :websocket-host "localhost"
                         :compiler { :main 'example.core
                                    :asset-path "js/out"
                                    :libsers ["libs_src" "libs_sscr/tweaky.js"]
                                    :output-to "resources/public/js/example.js"
                                    :output-dir "resources/public/js/out"
                                    :libs ["libs_src" "libs_sscr/tweaky.js"]
                                    ;; :externs ["foreign/wowza-externs.js"]
                                    :foreign-libs [{:file "foreign/wowza.js"
                                                    :provides ["wowzacore"]}]
                                    ;; :recompile-dependents true
                                    :optimizations :none}
                         :on-jsload      'example.core/fig-reload
                         :on-message     'example.core/on-message
                         :open-urls ["http://localhost:3449/index.html"
                                     "http://localhost:3449/index.html"
                                     "http://localhost:3449/index.html"
                                     "http://localhost:3449/index.html"
                                     "http://localhost:3449/index.html"]
                         :debug true
                         }
                        
                        :compiler { :main 'example.core
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/example.js"
                                   :output-dir "resources/public/js/out"
                                   :libs ["libs_src" "libs_sscr/tweaky.js"]
                                   ;; :externs ["foreign/wowza-externs.js"]
                                   :foreign-libs [{:file "foreign/wowza.js"
                                                      :provides ["wowzacore"]}]
                                   ;; :recompile-dependents true
                                   :optimizations :none}}
                       { :id "example"
                        :source-paths ["src" "dev" "tests" "../support/src"]
                        :notify-command ["notify"]
                        :figwheel
                        { :websocket-host "localhost"
                         :on-jsload      'example.core/fig-reload
                         :on-message     'example.core/on-message
                         :open-urls ["http://localhost:3449/index.html"]
                         :debug true
                         }
                        :compiler { :main 'example.core
                                   :asset-path "js/out"
                                   :output-to "resources/public/js/example.js"
                                   :output-dir "resources/public/js/out"
                                   :libs ["libs_src" "libs_sscr/tweaky.js"]
                                   ;; :externs ["foreign/wowza-externs.js"]
                                   :foreign-libs [{:file "foreign/wowza.js"
                                                   :provides ["wowzacore"]}]
                                   ;; :recompile-dependents true
                                   :optimizations :none}}]}
  )
