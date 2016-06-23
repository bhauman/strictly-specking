(ns strictly-specking.ansi-util
  (:require
   [clojure.string :as str]
   [clansi.core :as ansi]))

(defmacro with-ansi-when [b & body]
  `(if ~b
     (ansi/with-ansi ~@body)
     (do ~@body)))

(def ansi-code? ansi/ANSI-CODES)

(defmacro print-color-text [codes body]
  `(if ansi/*use-ansi*
     (do
       (.write ^java.io.Writer *out* (apply str (map ansi/ansi ~codes)))
       ~body
       (.write ^java.io.Writer *out* (ansi/ansi :reset)))
     ~body))


