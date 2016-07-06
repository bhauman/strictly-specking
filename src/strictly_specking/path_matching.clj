(ns strictly-specking.path-matching
  (:require
   [clojure.spec :as s]
   [clojure.set :as set]
   [clojure.pprint :as pp]
   [strictly-specking.strict-keys :as strict-impl]))



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
  [e possible-paths]
  ;; first is the value valid for this key-spec?
  (if-let [paths (not-empty (filter (fn [x]
                                      (s/valid? (-> x last :ky-spec)
                                                (get (:val e)
                                                     (:strictly-specking.core/unknown-key e))))
                                    possible-paths))]
    paths
    ;; if not is this a map and does it fuzzy conform
    (if-let [score-paths (not-empty (keep (fn [x]
                                            (let [vl (get (:val e)
                                                          (:strictly-specking.core/unknown-key e))]
                                              (when (map? vl)
                                                [(strict-impl/fuzzy-conform-score-spec-key
                                                  (-> x last :ky-spec)
                                                  vl)
                                                 x])))
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

(declare path-match)

(defn path-match-helper [possible-path kv-list]
  (->> kv-list
      (mapcat
       #(map (partial cons (first %))
             (path-match (rest possible-path) (second %))))
      distinct))

(defn path-match [possible-path data]
  (let [p (first possible-path)
        match-next (partial path-match-helper possible-path)]
    (if (or (nil? data) (nil? p))
      [[]]
      (or
       (condp = (:ky p)
         :strictly-specking.core/int-key
         (when (sequential? data)
           (match-next (map-indexed vector data)))
         :strictly-specking.core/pred-key
         (when (and (map? data)
                    (not= (:ky-pred-desc p) ::s/any))
           (match-next data))
         (and (map? data) (contains? data (:ky p))
              (match-next (select-keys data [(:ky p)]))))
       [[]]))))

(defn rank-path-matches [possible-path data]
  (->> (path-match possible-path data)
       (filter #(not= (last %)
                      (-> possible-path last :ky)))
       (map (fn [p] [(/ (count p) (count possible-path)) p possible-path]))
       (sort-by (comp - first))))

(defn path-matches [possible-paths data]
  (->> possible-paths
       (mapcat
        #(rank-path-matches % data))))

(defn fill-out-path [partial-path possible-path]
  [(concat partial-path
          (map
           :ky
           (drop (count partial-path) possible-path)))
   possible-path])

(defn best-possible-path [possible-paths data]
  (->> (path-matches possible-paths data)
       (sort-by (comp - first))
       first
       rest
       (apply fill-out-path)))


;; TODO we can also sample from the original structure
(defn generate-path-structure
  "Generates a data structure to demonstrate where to place a key"
  [data path v]
  (let [p (first path)]
    (cond
      (or (empty? path) (nil? path)) v
      (nil? data)
      (let [nxt (generate-path-structure nil (rest path) v)]
        (if (or (integer? p) (= :strictly-specking.core/int-key p))
          [nxt]
          (hash-map ((some-fn {:strictly-specking.core/pred-key :__any_keyword} identity) p) nxt)))
      :else
      (when-let [f (cond
                     ;; This is where you would do sampling
                     (map? data)       #(hash-map p %)
                     (vector? data)     vector
                     (set? data)        (comp set vector)
                     (sequential? data) list)]
        (f (generate-path-structure (get data p) (rest path) v))))))

(defn generate-path-structure-path
  "Generates a data structure to demonstrate where to place a key"
  [data path]
  (let [p (first path)]
    (cond
      (or (empty? path) (nil? path)) '()
      (nil? data)
      (let [nxt (generate-path-structure-path nil (rest path))]
        (if (or (integer? p) (= :strictly-specking.core/int-key p))
          (cons 0 nxt)
          (cons p nxt)))
      :else
      (when-let [f (cond
                     ;; This is where you would do sampling
                     (map? data)        (partial cons p)
                     (vector? data)     (partial cons 0)
                     (set? data)        (partial cons 0)
                     (sequential? data) (partial cons 0))]
        (f (generate-path-structure-path (get data p) (rest path)))))))

#_(generate-path-structure-path  {:a {:b [{:c {}}]}} [:a :b 1 :c ::int-key ::pred-key])

#_(best-possible-path
   ['({:ky-spec :strictly-specking.test-schema/cljsbuilds, :ky :cljsbuild}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/pred-key,
       :ky-pred-desc :strictly-specking.test-schema/string-or-named}
      {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})
    
    '({:ky-spec :cljsbuild.lein-project.require-builds/cljsbuild, :ky :cljsbuild}
      {:ky-spec :strictly-specking.test-schema/builds, :ky :builds}
      {:ky :strictly-specking.core/int-key}
      {:ky-spec :strictly-specking.test-schema/figwheel, :ky :figwheel})]
   '{:cljsbuild
     {:builds
      {:dev
       1
       :cow 1}}}
 )


(comment
  
  (def test-error
    '{:path
      [:cljsbuild
       :builds
       :builds-map
       1
       :figwheel
       :figwheel-client-options
       :unknown-key
       :source-map],
      :pred
      #{:on-message :on-compile-warning :on-jsload :websocket-host :reload-dependents
        :on-compile-fail :debug :heads-up-display :build-id :websocket-url :before-jsload
        :load-warninged-code :eval-fn :devcards :retry-count :autoload :open-urls :on-cssload},
      :val
      {:websocket-host "localhost",
       :on-jsload example.core/fig-reload,
       :on-message example.core/on-message,
       :open-urls
       ["http://localhost:3449/index.html"
        "http://localhost:3449/index.html"
        "http://localhost:3449/index.html"
        "http://localhost:3449/index.html"
        "http://localhost:3449/index.html"],
       :source-map true,
       :debug true},
      :via
      [:strictly-specking.test-schema/lein-project-with-cljsbuild
       :cljsbuild.lein-project.require-builds/cljsbuild
       :cljsbuild.lein-project/cljsbuild
       :strictly-specking.test-schema/builds
       :strictly-specking.test-schema/build-config
       :strictly-specking.test-schema/figwheel],
      :in [:cljsbuild :builds :dev :figwheel],
      :strictly-specking.core/unknown-key :source-map,
      :strictly-specking.core/error-type :strictly-specking.core/unknown-key,
      :strictly-specking.core/root-data
      {:cljsbuild
       {:builds
        {:dev
         {:id "example-admin",
          :source-paths ["src" "dev" "tests" "../support/src"],
          :assert true,
          :figwheel
          {:websocket-host "localhost",
           :on-jsload example.core/fig-reload,
           :on-message example.core/on-message,
           :open-urls
           ["http://localhost:3449/index.html"
            "http://localhost:3449/index.html"
            "http://localhost:3449/index.html"
            "http://localhost:3449/index.html"
            "http://localhost:3449/index.html"],
           :source-map true,
           :debug true},
          :compiler
          {:main example.core,
           :asset-path "js/out",
           :output-to "resources/public/js/example.js",
           :output-dir "resources/public/js/out",
           :libs ["libs_src" "libs_sscr/tweaky.js"],
           :foreign-libs [{:file "foreign/wowza.js", :provides ["wowzacore"]}],
           :optimizations :whitespace}}}}}})

  (def test-error2
    '{:path [:cljsbuild :builds :builds-map 1 :compiler :unknown-key :figwheel],
      :pred
      #{:output-dir :closure-defines :static-fns :dump-core :externs :ups-libs :optimize-constants
        :cache-analysis :modules :elide-asserts :language-out :optimizations :recompile-dependents
        :source-map-path :closure-extra-annotations :ups-foreign-libs :parallel-build :verbose
        :preloads :source-map-inline :anon-fn-naming-policy :output-to :source-map-timestamp
        :preamble :asset-path :print-input-delimiter :output-wrapper :ups-externs :hashbang
        :source-map :watch-fn :foreign-libs :libs :target :pseudo-names :devcards :external-config
        :compiler-stats :main :pretty-print :closure-output-charset :language-in :warning-handlers
        :emit-constants},
      :val
      {:main example.core,
       :asset-path "js/out",
       :output-to "resources/public/js/example.js",
       :output-dir "resources/public/js/out",
       :libs ["libs_src" "libs_sscr/tweaky.js"],
       :foreign-libs [{:file "foreign/wowza.js", :provides ["wowzacore"]}],
       :optimizations :whitespace,
       :figwheel
       {:websocket-host 1 #_"localhost",
        :on-jsload example.core/fig-reload,
        :on-message example.core/on-message,
        :open-urls
        ["http://localhost:3449/index.html"
         "http://localhost:3449/index.html"
         "http://localhost:3449/index.html"
         "http://localhost:3449/index.html"
         "http://localhost:3449/index.html"],
        :source-map true,
        :debug true}},
      :via
      [:strictly-specking.test-schema/lein-project-with-cljsbuild
       :cljsbuild.lein-project.require-builds/cljsbuild
       :cljsbuild.lein-project/cljsbuild
       :strictly-specking.test-schema/builds
       :strictly-specking.test-schema/build-config
       :strictly-specking.test-schema/compiler],
      :in [:cljsbuild :builds :dev :compiler],
      :strictly-specking.core/unknown-key :figwheel,
      :strictly-specking.core/error-type :strictly-specking.core/unknown-key,
      :strictly-specking.core/root-data
      {:cljsbuild
       {:builds
        {:dev
         {:id "example-admin",
          :source-paths ["src" "dev" "tests" "../support/src"],
          :assert true,
          :compiler
          {:main example.core,
           :asset-path "js/out",
           :output-to "resources/public/js/example.js",
           :output-dir "resources/public/js/out",
           :libs ["libs_src" "libs_sscr/tweaky.js"],
           :foreign-libs [{:file "foreign/wowza.js", :provides ["wowzacore"]}],
           :optimizations :whitespace,
           :figwheel
           {:websocket-host "localhost",
            :on-jsload example.core/fig-reload,
            :on-message example.core/on-message,
            :open-urls
            ["http://localhost:3449/index.html"
             "http://localhost:3449/index.html"
             "http://localhost:3449/index.html"
             "http://localhost:3449/index.html"
             "http://localhost:3449/index.html"],
            :source-map true,
            :debug true}}}}}}})
  )
