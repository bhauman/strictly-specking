(ns strictly-specking.test-schema
  (:require
   [clojure.spec :as s]
   [strictly-specking.cljs-options-schema]
   [strictly-specking.core :refer [strict-keys
                                   def-key
                                   non-blank-string?]
    :as ss]))

;; for development - not a crime if it happens in production
(ss/reset-duplicate-keys)

(def-key ::string-or-symbol (some-fn non-blank-string? symbol?))

(def-key ::string-or-named  (some-fn non-blank-string? keyword? symbol?))

#_(s/conform ::string-or-named :asdfasdf)

;; * Figwheel Configuration

;; ** In the Leiningen project.clj

;; The most common way to configure lein-figwheel is is in the
;; Leiningen project.clj which is normally found at the root of your
;; clojure project directory

;; *** :figwheel Figwheel System Options

;; To supply configuration options to the figwheel system place the
;; following options in a Map under the :figwheel key at the root or
;; the project.clj

(def-key :figwheel.lein-project/figwheel
  (strict-keys
   :opt-un
   [::http-server-root
    ::server-port
    ::server-ip
    ::css-dirs
    ::ring-handler
    ::builds-to-start
    ::server-logfile
    ::open-file-command
    ::repl
    ::nrepl-port
    ::nrepl-host
    ::nrepl-middleware
    ::validate-config
    ::load-all-builds
    ::ansi-color-output
    ::builds
    ::hawk-options])
  "A Map of options that determine the behavior of the Figwheel system.

  :figwheel {
    :css-dirs [\"resources/public/css\"]
  }")

(def-key ::http-server-root  string?

  "Figwheel relies on the compojure.route/resources handler to serve
static files. This serves resources on the classpath from a specific
subdirectory. The default is \"public\" so any file on the resource path
in a \"public\" subdirectory is capable of being served.

You can change the default \"public\" to something else. But you can't
have a blank :http-server-root this can lead to insidious compiler
problems because the compiler also looks up resources when it looks
for source files.
Default: \"public\"

  :http-server-root \"public\"")

(def-key ::server-port       integer?

  "An integer that the figwheel HTTP and Websocket server should bind.
Default: 3449

  :server-port 3000")

(def-key ::server-ip         string?
  "The network interface that the figwheel server will listen on. This is
useful if you don't want to use a public network interface.
Default: \"localhost\"

  :server-ip \"0.0.0.0\"")

(def-key ::css-dirs          (s/+ string?)

  "A vector of paths from the project root to the location of your css
files. These files will be watched for changes and the figwheel client
will attempt to reload them.

  :css-dirs [\"resources/public/css\"]")

(def-key ::ring-handler      ::string-or-named

  "If you want to embed a ring handler into the figwheel http-kit server;
this is for simple ring servers, if this doesn't work for you just run
your own server.
Default: Off

  :ring-handler example.core/my-server-handler")

;; co-dependency - needs to be in builds
(def-key ::builds-to-start   (s/* ::string-or-named)

  "A vector of build ids that you would like figwheel to start building
when you invoke lein figwheel without arguments.

  :builds-to-start [\"dev\" \"test\"]")

(def-key ::server-logfile    string?)

(def-key ::open-file-command string?
  "A path to an executable shell script that will be passed a file and
line information for a particular compilation error or warning.

A script like this would work
ie. in  ~/bin/myfile-opener
#! /bin/sh
emacsclient -n +$2 $1

  :open-file-command \"myfile-opener\"")

(def-key ::repl              boolean?

  "A Boolean value indicated wether to run a ClojureScript REPL after the
figwheel process has launched.
Default: true

  :repl false")

(def-key ::nrepl-port        integer?

  "An integer indicating that you would like figwheel to launch nREPL
from within the figwheel process and what port you would like it to
launch on.
Default: off

  :nrepl-port 7888")

(def-key ::nrepl-host        string?

  "If the :nrepl-port is provided Figwheel will launch an nREPL server
into the figwheel compilation process.  :nrepl-host is a string which
specifies which local network interface you want to launch the server on.

  :nrepl-host \"localhost\"")

(def-key ::nrepl-middleware  (s/+ ::string-or-named)

  "A vector of strings indicating the nREPL middleware you want included
when nREPL launches.

  :nrepl-middleware [\"cider.nrepl/cider-middleware\" \"cemerick.piggieback/wrap-cljs-repl\"]")

(def-key ::validate-config   boolean?

  "Set this to false to skip the configuration validation.
Default: true

  :validate-config false")

(def-key ::load-all-builds   boolean?

  "A Boolean value that specifies wether or not to load all the
ClojureScript builds that are available in your config.  When these
builds are loaded all of their source paths are added to the classpath.

This can result in load conflicts if you are overloading a Clojure
namespace.

If your project.clj contains conflicting builds, you can choose to
only load the builds specified on the command line or in
:builds-to-start by setting :load-all-builds to false.")

(def-key ::ansi-color-output boolean?

  "Figwheel makes an effort to provide colorful text output. If you need
to prevent ANSI color codes in figwheel output set :ansi-color-output
to false.  Default: true

  :ansi-color-output false")

(def-key ::hawk-options (s/map-of #{:watcher} #{:barbary :java :polling})

  "If you need to watch files with polling instead of FS events. This can
be useful for certain docker environments.

  :hawk-options {:watcher :polling}" )

(def-key ::reload-clj-files
  (s/or
   :bool boolean?
   :suffix-map
   (strict-keys
    :opt-un
    [::clj
     ::cljs]))
  
  "Figwheel naively reloads clj and cljc files on the :source-paths.  
It doesn't reload clj dependent files like tools.namspace. 

Figwheel does note if there is a macro in the changed clj or cljc file
and then marks any cljs namespaces that depend on the clj file for
recompilation and then notifies the figwheel client that these
namespaces have changed.

If there is no macro in the clj/cljc namespace figwheel marks all cljs files
for recompilation.
  
If you want to disable this behavior:

  :reload-clj-files false

Or you can specify which suffixes will cause the reloading

  :reload-clj-files {:clj true :cljc false}")

;; the following are only for the :reload-clj-files Map
(def-key ::clj  boolean?)
(def-key ::cljs boolean?)

;; *** Build Configurations

;; The primary configuration information for Figwheel are the build
;; configuration definitions.

;; These determine
;; - what source files the CLJS compiler will compile
;; - how they should be compiler
;; - where the compiled files should be placed

;; Build configuration can be found in the project.clj under either
;; the :cljsbuild > :builds key or the :figwheel > :builds key.

;; Because lein-figwheel needs ClojureScript build configurations just
;; like lein-cljsbuild Figwheel will re-use the build configurations
;; in :cljsbuild > :builds. This is the most common place to put your
;; build configurations.

(def-key :cljsbuild.lein-project/cljsbuild
  (strict-keys
   :opt-un [::builds
            ::repl-listen-port
            ::repl-launch-commands
            ::test-commands
            ::crossovers
            ::crossover-path
            ::crossover-jar]))

;; spec out other lein cljsbuild options to provide comprehensive
;; configuration validation
(def-key ::repl-listen-port      integer?)
(def-key ::repl-launch-commands  (s/map-of ::string-or-named (s/* ::string-or-named)))
(def-key ::test-commands         (s/map-of ::string-or-named (s/* ::string-or-named)))
(def-key ::crossovers            (s/* ::s/any))
(def-key ::crossover-path        (s/* ::s/any))
(def-key ::crossover-jar         boolean?)

;; If :cljsbuild > :builds is not present you must have :figwheel > :builds

;; The ::builds key can be either a map of ::build-configs or vector of
;; ::build-configs. Again it can be placed under :cljsbuild or :figwheel
;; it also can be found at the top level of a figwheel.edn file

(def-key ::builds 
  (s/or                               ;; wait until merge works
   :builds-vector (s/+ ::build-config-require-id)
   :builds-map    (s/map-of ::string-or-named ::build-config))
  "A Vector or Map of ClojureScript Build Configurations.

  :builds [{:id \"dev\"
            :source-paths [\"src\"]
            :figwheel true
            :compiler {:main example.core
                       :asset-path \"js/out\"
                       :output-to \"resources/public/example.js\"
                       :output-dir \"resources/public/out\"}}]

   or

  :builds {:dev {:source-paths [\"src\"]
                 :figwheel true
                 :compiler {:main example.core
                            :asset-path \"js/out\"
                            :output-to \"resources/public/example.js\"
                            :output-dir \"resources/public/out\"}}}")

(def-key ::build-config
  (strict-keys
   :opt-un
   [::id
    ::notify-command
    ::jar
    ::incremental
    ::assert
    ::warning-handlers
    ::figwheel]
   :req-un
   [::source-paths
    ::compiler])

  "A Map of options that specifies a ClojureScript 'build'

   {:id \"dev\"
    :source-paths [\"src\"]
    :figwheel true
    :compiler {:main example.core
               :asset-path \"js/out\"
               :output-to \"resources/public/example.js\"
               :output-dir \"resources/public/out\"}}")

;; When you use a vector to define your :builds you have to supply an :id

(def-key ::build-config-require-id
  (s/and
   ::build-config 
   (s/keys
    :req-un [::id]))
  
  "A Map of options that specifies a ClojureScript 'build'

  {:id \"dev\"
   :source-paths [\"src\"]
   :figwheel true
   :compiler {:main example.core
              :asset-path \"js/out\"
              :output-to \"resources/public/example.js\"
              :output-dir \"resources/public/out\"}}")

;; **** Build Config options

(def-key ::id               ::string-or-named

  "A Keyword, String or Symbol that identifies this build.

  :id \"dev\"")

(def-key ::source-paths     (s/+ string?)

  "A vector of paths to your cljs source files. These paths should be
relative from the root of the project to the root the namespace.
For example, if you have an src/example/core.cljs file that contains a
example.core namespace, the source path to this file is \"src\"

  :source-paths [\"src\"]")

(def-key ::figwheel
  (s/or
   :bool boolean?
   :figwheel-client-options
   (strict-keys
    :opt-un
    [::build-id
     ::websocket-host
     ::websocket-url
     ::on-jsload
     ::before-jsload
     ::on-cssload
     ::on-message
     ::on-compile-fail
     ::on-compile-warning
     ::reload-dependents
     ::debug
     ::autoload
     ::heads-up-display
     ::load-warninged-code
     ::retry-count
     ::devcards
     ::eval-fn
     ::open-urls]))
  "Either the Boolean value true or a Map of options to be passed to the
figwheel client. Supplying a true value or a map indicates that you
want the figwheel client code to be injected into the build.

  :figwheel true")

(def-key ::compiler ::strictly-specking.cljs-options-schema/compiler-options
  "The options to be forwarded to the ClojureScript Compiler
  
Please refer to   

  :compiler {:main example.core
             :asset-path \"js/out\"
             :output-to \"resources/public/example.js\"
             :output-dir \"resources/public/out\"}")

(def-key ::notify-command   (s/+ string?)

  "If a :notify-command is specified, it will be called when compilation
succeeds or fails, and a textual description of what happened will be
appended as the last argument to the command. If a more complex
command needs to be constructed, the recommendation is to write a
small shell script wrapper.
Default: nil (disabled)

  :notify-command [\"growlnotify\" \"-m\"]")

;; we are supporting lein-cljsbuild build-config options to provide
;; more comprehensive validation

(def-key ::jar              boolean?)
(def-key ::incremental      boolean?)
(def-key ::assert           boolean?)
(def-key ::warning-handlers (s/+ ::s/any))

;; **** Figwheel Client Options

;; Figwheel client options are provided under the :figwheel key in a
;; build configuration

(def-key ::build-id             non-blank-string?

  "A Keyword, String or Symbol that identifies this build.

  :build-id \"dev\"")

(def-key ::websocket-host (s/or :string non-blank-string?
                                :host-option #{:js-client-host :server-ip :server-hostname})
  
 "A String specifying the host part of the Figwheel websocket URL. If you have
JavaScript clients that need to access Figwheel that are not local, you can
supply the IP address of your machine here, or you can specify one of the
following keywords to have figwheel determine the string for you.
  :js-client-host  -- will use js/window.location.host
  :server-ip       -- will use local IP address of figwheel server
  :server-hostname -- will do a local hostname lookup on figwheel server
Default: \"localhost\"

  :websocket-host :server-ip")

(def-key ::websocket-url        non-blank-string?)

(def-key ::on-jsload            ::string-or-named

  "A String or Symbol representing a client side ClojureScript function
to be invoked after new code has been loaded.
Default: Off

  :on-jsload \"example.core/fig-reload\"")

(def-key ::heads-up-display     boolean?

  "Show a notification in the browser on each refresh.
Default: true

  :heads-up-display false")

(def-key ::load-warninged-code  boolean?

  "If there are warnings in your code emitted from the compiler, figwheel
does not refresh. If you would like Figwheel to load code even if
there are warnings generated set this to true.
Default: false

  :load-warninged-code true")

(def-key ::open-urls            (s/+ string?)

  "A Vector of URLs that you would like to have opened at the end of the
initial compile. These URLs must be Strings.

This is great for opening the host page for the target build and other
helpful websites like http://cljs.info

These urls will be opened with clojure.java.browse/browse-url.
Default: nil (disabled)

  :open-urls")

;; TODO fill out the docs below

(def-key ::before-jsload        ::string-or-named)
(def-key ::on-cssload           ::string-or-named)
(def-key ::on-message           ::string-or-named)
(def-key ::on-compile-fail      ::string-or-named)
(def-key ::on-compile-warning   ::string-or-named)
(def-key ::eval-fn              ::string-or-named)

(def-key ::reload-dependents    boolean?)
(def-key ::debug                boolean?)
(def-key ::autoload             boolean?)
(def-key ::devcards             boolean?)
(def-key ::retry-count          integer?)


;; * Conditional Top Level Specs

;; *** Leiningen Project
;; we have different situations

;; normal situation is :cljsbuild with required builds

(defn lein-project-spec [project]
  ;; TODO this needs fuzzy key detection
  (cond
    (get-in project [:figwheel :builds]) ::lein-project-with-figwheel-builds
    (not (get-in project [:cljsbuild]))  ::lein-project-only-figwheel
    :else ::lein-project-with-cljsbuild))

(def-key ::lein-project-with-cljsbuild
  (strict-keys
   :opt-un [:figwheel.lein-project/figwheel]
   :req-un [:cljsbuild.lein-project.require-builds/cljsbuild]))

;; if only figwheel is available

(def-key ::lein-project-only-figwheel
  (strict-keys
   :req-un [:figwheel.lein-project.require-builds/figwheel]))

;; if figwheel is available with builds in it

(def-key ::lein-project-with-figwheel-builds
  (strict-keys
   :opt-un [:figwheel.lein-project/figwheel]
   ;; don't require builds in cljsbuild
   :req-un [:cljsbuild.lein-project/cljsbuild]))

(def-key :cljsbuild.lein-project.require-builds/cljsbuild
  :cljsbuild.lein-project/cljsbuild
  ;; wait for merge to not propogate conformed values
  #_(s/merge
     
     (s/keys
      :req-un [::builds])))

(def-key :figwheel.lein-project.require-builds/figwheel
  :figwheel.lein-project/figwheel
  ;; wait for merge to not propogate conformed values
  #_(s/merge

   (s/keys
    :req-un [::builds])))

;; ** figwheel.edn

(def-key ::figwheel-edn :figwheel.lein-project.require-builds/figwheel
  "If a figwheel.edn file is present at the root of your directory
figwheel will use this file as a configuration source. 

The structure of the EDN in figwheel.edn file is the same as the
structure of the top level :figwheel key in the project.clj

Example figwheel.edn file

  {:server-port 4000
   :http-server-root \"public\"
   :css-dirs [\"resources/public/css\"]
   :builds {:dev {:id \"dev\"
                  :source-paths [\"src\"]
                  :figwheel true
                  :compiler {:main example.core
                             :asset-path \"js/out\"
                             :output-to \"resources/public/example.js\"
                             :output-dir \"resources/public/out\"}}}}")

;; ** Figwheel Internal 

;; co-dependency - must exist in all-builds
(def-key ::build-ids        (s/* ::string-or-named))

(def-key ::figwheel-options :figwheel.lein-project/figwheel)

(def-key ::all-builds       (s/+ ::build-config-require-id))

(def-key ::figwheel-internal-config
  (strict-keys
   :req-un [::all-builds]
   :opt-un [::figwheel-options
            ::build-ids]))

(comment
  (def test-data
    { :cljsbuild {
                  :build [{:id "example-admin"
                            :source-paths ["src" "dev" "tests" "../support/src"]
                            :notify-command ["notify"]
                            :assert true
                            :figwheel
                            {:websocket-host "localhost"
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
                                       :optimizations :none}}]}})

  (ss/dev-print (s/explain-data ::lein-project-with-cljsbuild
                                test-data)
                test-data
                nil)
  
  (s/explain ::lein-project-with-cljsbuild
             test-data)

  (s/explain ::build-config-require-id (get-in test-data [:cljsbuild :builds 0]))

  (s/def ::string  (s/or :string string?))
  
  (s/def ::myid ::string)
  
  (s/explain (s/merge
              (s/keys :opt-un [::myid])
              (s/spec (fn [x] (prn x) true))
              (s/keys :req-un [::myid]))
             {:myid "asdf"})

  (s/explain (s/and
              (s/or :int integer?)
              (s/spec (fn [x] (prn x)
                        true))
              )
             5)
  
  )

