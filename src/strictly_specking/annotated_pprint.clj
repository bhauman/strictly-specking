(ns strictly-specking.annotated-pprint
  (:require
   [strictly-specking.ansi :as cl]
   [clojure.pprint :as pp]
   [clojure.string :as string]))

(defn use-method
  "Installs a function as a new method of multimethod associated with dispatch-value. "
  [^clojure.lang.MultiFn multifn dispatch-val func]
  (. multifn addMethod dispatch-val func))

(defn into-multimethod
  "Copies all of the methods from a source multimethod into the target multimethod.
This makes it easier to override pprint functionality for certain types."
  [target source]
  {:pre [(= clojure.lang.MultiFn (class target))
         (= clojure.lang.MultiFn (class source))]}
  (doseq [[dispatch-val func] (.getMethodTable ^clojure.lang.MultiFn source)]
    (use-method target dispatch-val func))
  target)

(defmulti error-path-dispatch
  "The pretty print dispatch function for printing paths to errors in maps"
  class)

(defrecord CommentPointer [text])

(into-multimethod error-path-dispatch pp/simple-dispatch)

(defn print-colored [colors obj]
  {:pre [(or (nil? colors)
             (and
              (sequential? colors)
              (not-empty colors)
              (every? cl/sgr colors)))]}
  (if colors
    (cl/print-color-text
     colors
     (pp/write-out obj))
    (pp/write-out obj)))

(def orig-pprint-map (.getMethod pp/simple-dispatch clojure.lang.IPersistentMap))

;; should create a dispatch that uses the normal method for maps
;; and this method for annotated maps
(defn pprint-map-with-pointer [amap]
  (let [comments (or (-> amap meta :comments) {})
        comments? (not-empty comments)]
    (pp/pprint-logical-block
     :prefix "{" :suffix "}"
     (pp/print-length-loop
      [aseq (seq amap)]
      (when-let [[k v] (first aseq)]
        (let [{:keys [comment key-colors value-colors comment-colors]
               :as current-comment} (get comments k)]
          (pp/pprint-logical-block
           (print-colored key-colors k)
           (.write ^java.io.Writer *out* " ")
           (pp/pprint-newline :linear)
           (print-colored value-colors
                          (if (and current-comment
                                   (coll? v)
                                   (-> amap meta :abbrev))
                            (vary-meta v assoc :abbrev :tight)
                            v)))
          (if comment
            (do
              (pp/pprint-newline :mandatory)
              (print-colored comment-colors (CommentPointer. comment))
              (pp/pprint-newline :mandatory))
            (when (next aseq)
              (.write ^java.io.Writer *out* " ")
              (pp/pprint-newline (if comments? #_(and comments?
                                                   (not (-> amap meta :abbrev))
                                                   ) 
                                   :mandatory :linear))))
          (when (next aseq)
            (recur (next aseq)))))))))

(defn abbrev-pprint-map-with-pointer [amap]
  (if-let [abr-level  (-> amap meta :abbrev)]
    (let [length (if (= :tight abr-level) 2 4)
          level  (if (= :tight abr-level) 2 3)]
      (binding [*print-level* (+ level
                                 (or @(resolve 'clojure.pprint/*current-level*) 0))
                *print-length* length]
        (pprint-map-with-pointer amap)))
    (pprint-map-with-pointer amap)))

(let [pprint-message (pp/formatter-out "^---- ~5I~@{~a~^ ~_~}~0I")]
  (defn pprint-comment-pointer [comm-point]
    (apply pprint-message (string/split (:text comm-point) #"\n"))))

(use-method error-path-dispatch clojure.lang.IPersistentMap abbrev-pprint-map-with-pointer)
(use-method error-path-dispatch CommentPointer pprint-comment-pointer)

;; given a path and a datastructure produce a datastructure to
;; be printed to represent the display of the bad configuration

(defn path-only-data-structure [data path f]
  (if-let [p (first path)]
    (cond
      (map? data)
      (-> data
          (select-keys [p])
          (assoc-in [p] (path-only-data-structure (get data p) (rest path) f)))
      ;; this should never be exercised
      (set? data)
      #{(path-only-data-structure (get data p) (rest path) f)}
      (vector? data)
      (vector (path-only-data-structure (get data p) (rest path) f))
      (sequential? data)
      (list (path-only-data-structure (get data p) (rest path) f))
      :else data)
    (f data)))

(defn annotate-path-only [data path annotation]
  (path-only-data-structure data path #(vary-meta % merge annotation)))

(defn annotate-path [data path annotation]
  (update-in data path #(vary-meta % merge annotation)))

(defn annotate-paths [data path-to-annotation-map]
  (reduce #(annotate-path %1 (first %2) (second %2))
          data path-to-annotation-map))

(comment
  (def test-data
    {:cljsbuild {:assert true
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
                                      :optimizations :none}}]}}
    )

  (cl/with-color-when true
    (pp/with-pprint-dispatch error-path-dispatch
      (pp/pprint
       (annotate-paths test-data
                      {[:cljsbuild :builds 0 :figwheel]
                       {:abbrev true
                        :comments {:compiler {;:value-colors [:red]
                                              :key-colors [:green]
                                              :value-colors [:red]
                                              :comment-colors [:magenta]
                                              :comment " is missing yep "#_(CommentPointer. )
                                             }}}})
       
       )))
  
  #_(cl/with-color
    (pp/with-pprint-dispatch error-path-dispatch
      (pp/pprint {:cljsbuild
                  {
                   :builds [{ :id "example"
                             :source-paths 1 #_["src" #_"dev" #_"tests" #_"../support/src"]
                             :notify-command ["notify"]
                             :figwheel (with-meta
                                         { :websocket-host "localhost"
                                          :on-jsload      'example.core/fig-reload
                                          :on-message     'example.core/on-message
                                        ; :open-urls ["http://localhost:3449/index.html"]
                                        ; :debug true
                                          }
                                         {:comments {:on-jsload
                                                     {:value-comment-colors [:red]
                                                      :key-colors [:green]
                                                      :value (CommentPointer. "hey asdf asdf asdf asdf\n asdf asdf as df as df asd f asd fa sd fa \n sd f asd fa sd f asd f asd fasd fa sdf a sdf asdf a sdf a\n sdf a sdf a sdf")}}})
                             :compiler { :main 'example.core
                                        :asset-path "js/out"
                                        :output-to "resources/public/js/example.js"
                                        :output-dir "resources/public/js/out"
                                        :libs ["libs_src" "libs_sscr/tweaky.js"]
                                        ;; :externs ["foreign/wowza-externs.js"]
                                        :foreign-libs [{:file "foreign/wowza.js"
                                                        :provides ["wowzacore"]}]
                                        ;; :recompile-dependents true
                                        :optimizations :none}}]}})))
  
  )
