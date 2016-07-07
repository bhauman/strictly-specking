(ns strictly-specking.tasks
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :as shell]))


(let [reg #"(.*)\[clojure.spec\s+\:as\s+s](.*)"]
  (defn fix-spec-require [line]
    (if (re-matches reg line)
      (string/replace line
                      reg
                      (str "$1[strictly-specking.spec :as s]$2"))
      line)))

(def add-newline #(str % "\n"))

(defn fix-spec-require-in-file [file]
  (->> (line-seq (io/reader file))
       (mapv fix-spec-require)
       (string/join "\n")
       add-newline
       (spit (io/file file))))

(defn fix-spec-requires []
  (->> (file-seq (io/file "src/strictly_specking"))
       (concat (file-seq (io/file "test")))
       (filter #(.endsWith (str %) ".clj"))
       (filter #(.exists %))
       (mapv fix-spec-require-in-file)))

(defn standalone []
  (fix-spec-requires)
  
  )

(comment
  (fix-spec-requires)
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
    ":standalone" (fix-spec-requires)))
