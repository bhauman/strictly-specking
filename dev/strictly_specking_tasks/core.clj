(ns strictly-specking-tasks.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]))


(let [reg #"(.*)\[clojure.spec\s+\:as\s+s](.*)"]
  (defn standalone-spec-require [line]
    (if (re-matches reg line)
      (string/replace line
                      reg
                      (str "$1[strictly-specking.spec :as s]$2"))
      line)))

(let [reg #"(.*)\[strictly-specking.spec\s+\:as\s+s](.*)"]
  (defn fix-spec-require [line]
    (if (re-matches reg line)
      (string/replace line
                      reg
                      (str "$1[clojure.spec :as s]$2"))
      line)))

(def add-newline #(str % "\n"))

(defn fix-spec-require-in-file [f file]
  (->> (line-seq (io/reader file))
       (mapv f)
       (string/join "\n")
       add-newline
       (spit (io/file file))))

(defn fix-spec-requires [req-fn]
  (->> (file-seq (io/file "src/strictly_specking"))
       (concat (file-seq (io/file "test")))
       (concat (file-seq (io/file "dev-resources/test_specs")))
       (filter #(.endsWith (str %) ".clj"))
       (filter #(.exists %))
       (mapv req-fn)))

(defn output-spec-shim []
  (spit
   (io/file "src/strictly_specking/spec.clj")
   (->> '[(ns strictly-specking.spec
            (:refer-clojure :exclude [+ * and or cat def keys merge]))
          (load "/1.6_compat_spec/spec/gen")
          (load "/1.6_compat_spec/spec")]
        (mapv pr-str)
        (string/join "\n")
        add-newline)))

(defn standalone []
  (fix-spec-requires (partial fix-spec-require-in-file standalone-spec-require))
  (output-spec-shim))

(defn with-clojure-spec []
  (fix-spec-requires (partial fix-spec-require-in-file fix-spec-require))
  (.delete (io/file "src/strictly_specking/spec.clj")))

(comment
  (with-clojure-spec)
  (standalone)
  (fix-spec-require-in-file (io/file "src/strictly_specking/core.clj"))
  (fix-spec-require "   [clojure.spec :as s]")

  (re-matches
   
   )
  
  (string/replace "   [clojure.spec :as s]"
                  #"(.*)\[clojure.spec\s+\:as\s+s](.*)"
                  (str "$1[strictly-specing.spec :as s]$2")
 ))

(defn -main [command & args]
  (condp = command
    ":standalone" (standalone)
    ":with-clojure-spec" (with-clojure-spec))
  )

