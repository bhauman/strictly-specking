(ns strictly-specking.annotated-pprint
  (:require
   [strictly-specking.ansi-util :as ansi-util]
   [clojure.pprint :as pp]
   [clojure.string :as string]))

#_(remove-ns 'strictly-specking.annotated-pprint)

;; this is a library that allows you to print annotated EDN

;; TODO ensure that this works for set paths
;; TODO this can be improved to handle any general datastructure
;; TODO make this ClojureScript compatible
;; this will facilitate great runtime error reporting for Spec in Clojure and ClojureScript

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
              (not-empty colors)))]}
  (if colors
    (ansi-util/print-color-text
     colors
     (pp/write-out obj))
    (pp/write-out obj)))

;; these methods are private so ... have to fetch them this way
(def orig-pprint-map    (.getMethod pp/simple-dispatch clojure.lang.IPersistentMap))
(def orig-pprint-vector (.getMethod pp/simple-dispatch clojure.lang.IPersistentVector))
(def orig-pprint-list   (.getMethod pp/simple-dispatch clojure.lang.ISeq))
(def orig-pprint-set    (.getMethod pp/simple-dispatch clojure.lang.IPersistentSet))


;; should create a dispatch that uses the normal method for maps
;; and this method for annotated maps
(defn pprint-map-with-pointer* [amap]
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

(defn pprint-map-with-pointer [amap]
  (if (-> amap meta :comments)
    (pprint-map-with-pointer* amap)
    (orig-pprint-map amap)))

;; adjusting keys into view

(defn key-position [map k]
  (or (some identity
        (map-indexed (fn [i [a _]]
                       (when (= a k) i)) map))
      400))

(defn move-comments-into-view [amap]
  (let [length (or *print-length* 4)]
    (if-let [comments (-> amap meta :comments)]
      (if (or (<= (count amap) length)
              (every? (comp
                       (fnil (partial >= (dec length)) (inc length))
                       (partial key-position amap))
                      (keys comments)))
        amap
        (let [comment-parts (select-keys amap (keys comments))
              r (apply dissoc amap (keys comments))
              [comment-parts r] (if (and (>= (count r) 1)
                                         (<= (count comments) (dec length)))
                                  [(cons (first r) comment-parts) (rest r)]
                                  [comment-parts r])]
          (with-meta (into {} (reduce #(cons %2 %1) r (reverse comment-parts)))
            (meta amap))))
      amap)))

#_(move-comments-into-view (with-meta {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7}
                             {:comments {:b {:comment "yo"} :f {:comment "hi"}}}))
#_(> 3 (apply max (map (partial key-position {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7})
                     [:a :rr])))

(defn calc-map-length [amap]
  (max
   (if-let [comments (-> amap meta :comments)]
     (let [max-pos (apply max (map #(or (key-position amap %) 400)
                                   (keys comments)))]
       (if (> 9 max-pos)
         (+ 3 max-pos)
         (inc (count comments))))                        
     4)
   4))

#_(calc-map-length (with-meta {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7}
                     {:comments {:b {:comment "yo"} :d {:comment "hi"}}}))

(defn abbrev-pprint-map-with-pointer [amap]
  (if-let [abr-level  (-> amap meta :abbrev)]
    (let [length (if (= :tight abr-level) 2
                     (calc-map-length amap))
          level  (if (= :tight abr-level) 2 3)]
      (binding [*print-level* (+ level
                                 (or @(resolve 'clojure.pprint/*current-level*) 0))
                *print-length* length]
        (pprint-map-with-pointer (move-comments-into-view amap))))
    (pprint-map-with-pointer amap)))

(let [pprint-message (pp/formatter-out "^---- ~6I~@{~a~^ ~_~}~0I")]
  (defn pprint-comment-pointer [comm-point]
    (apply pprint-message (string/split (:text comm-point) #"\n"))))

(defn pprint-seq-with-pointer [pre suf avec comment-key-fn]
  (let [comments (or (-> avec meta :comments) {})
        abbrev   (-> avec meta :comments)
        comments? (not-empty comments)]
    ;;abbrev print-length can be 3 + max key
    (pp/pprint-logical-block
     :prefix pre :suffix suf
     (pp/print-length-loop
      [aseq (seq avec)
       c    0]
      (when aseq
        (let [v (first aseq)
              v (if (and abbrev (coll? v))
                  (vary-meta v assoc :abbrev :tight)
                  v)]
          (if-let [{:keys [comment value-colors comment-colors]
                    :as current-comment} (get comments (comment-key-fn v c))]
            (do
              (print-colored value-colors v)
              (pp/pprint-newline :mandatory)
              (print-colored comment-colors (CommentPointer. comment))
              #_(pp/pprint-newline :mandatory))
            (pp/write-out v))
          (when (next aseq)
            (.write ^java.io.Writer *out* " ")
            (pp/pprint-newline (if comments? :mandatory :linear))            
            (recur (next aseq) (inc c)))))))))

(defn abbrev-seq-with-pointer [pre suf a-seq comment-key-fn]
  (if-let [abr-level (-> a-seq meta :abbrev)]
    (let [maxk   (or
                  (when-let [comments (not-empty (-> a-seq meta :comments))]
                    (and (every? integer? (keys comments))
                         (reduce max (keys comments))))
                  0)
          length (if (= :tight abr-level) (+ maxk 2) (+ maxk 4))
          level  (if (= :tight abr-level) 2 3)]
      (binding [*print-level* (+ level
                                 (or @(resolve 'clojure.pprint/*current-level*) 0))
                *print-length* length]
        (pprint-seq-with-pointer pre suf a-seq comment-key-fn)))
    (pprint-seq-with-pointer pre suf a-seq comment-key-fn)))

(defn pprint-vector-with-pointer [avec]
  (if (-> avec meta :comments)
    (abbrev-seq-with-pointer "[" "]" avec (fn [_ c] c))
    (orig-pprint-vector avec)))

(defn pprint-set-with-pointer [a-set]
  (if (-> a-set meta :comments)
    (abbrev-seq-with-pointer "#{" "}" a-set (fn [_ c] c))
    (orig-pprint-set a-set)))

(defn pprint-simple-list-with-pointer [alis]
  (abbrev-seq-with-pointer "(" ")" alis (fn [_ c] c)))

(defn pprint-list-with-pointer [aseq]
  (if (-> aseq meta :comments)
    (pprint-simple-list-with-pointer aseq)
    (orig-pprint-list aseq)))

(use-method error-path-dispatch clojure.lang.IPersistentSet pprint-set-with-pointer)
(use-method error-path-dispatch clojure.lang.ISeq pprint-list-with-pointer)
(use-method error-path-dispatch clojure.lang.IPersistentVector pprint-vector-with-pointer)
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
  (path-only-data-structure data path
                            #(do
                               #_(prn path %)
                               (when (coll? %)
                                 (vary-meta % merge annotation)))))

(defn annotate-path [data path annotation]
  (if (empty? path)
    (vary-meta data merge annotation)
    (update-in data path #(vary-meta % merge annotation))))

(defn annotate-paths [data path-to-annotation-map]
  (reduce #(annotate-path %1 (first %2) (second %2))
          data path-to-annotation-map))

(defn pprint-notes [annotated-data]
  (pp/with-pprint-dispatch error-path-dispatch
      (pp/pprint annotated-data)))

#_(ansi-util/with-ansi-when true
    (pp/with-pprint-dispatch error-path-dispatch
      (pp/pprint
       (annotate-path-only test-data
                           [:cljsbuild :builds 0 :source-paths]
                           {:abbrev true
                            :comments {0 {
                                              :key-colors     [:green]
                                              :value-colors   [:bright]
                                              :comment-colors [:magenta]
                                              :comment " is screwwed"
                                              }
                                       10 {
                                                :key-colors [:green]
                                                :value-colors [:bright]
                                                :comment-colors [:magenta]
                                                :comment "is screwwed\n asdf  as df asdfads"
                                          }}}))))

(comment
  (def test-data
    {:cljsbuild {:assert true
                 :builds [{ :id "example-admin"
                           :source-paths ["src" "dev" "tests" "../support/src"
                                          "src" "dev" "tests" "../support/src"
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
                            } "dev" "tests" "../support/src"
                                          "src" "dev" "tests" "../support/src"
                                          "src" "dev" "tests" "../support/src"]
                           :source-paths-list '("src" "dev" "tests" "../support/src")
                           :source-paths-set #{"src" "dev" "tests" "../support/src"}
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

  (ansi-util/with-ansi-when true
    (pp/with-pprint-dispatch error-path-dispatch
      (pp/pprint
       (annotate-paths test-data
                      {[:cljsbuild :builds 0 :figwheel]
                       {:abbrev true
                        :comments {:compiler {;:value-colors [:red]
                                              :key-colors [:green]
                                              :value-colors [:red]
                                              :comment-colors [:magenta]
                                              :comment " is missing yep\n asdf asd f asdf asdf asdf "#_(CommentPointer. )
                                             }}}})
       
       )))
  
  #_(cl/with-ansi-when true
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
