(ns strictly-specking.ansi-util
  (:require
   [clojure.string :as str]
   [clansi.core :as ansi]))

;; I really prefer false as the default
;; I may just inline clansi here
#_(alter-var-root #'clansi.core/*use-ansi* (constantly false))

(defmacro with-ansi-when [b & body]
  `(if ~b
     (ansi/with-ansi ~@body)
     (do ~@body)))

(def ansi-code? ansi/ANSI-CODES)

(def ^:dynamic *print-styles*
  {:highlight   [:bright]
   :good        [:green]
   :good-pred   [:green]
   :good-key    [:green]      
   :bad         [:red]
   :bad-value   [:red]
   :error-key   [:red]
   :focus-key   [:bright]   
   :correct-key [:green]   
   :header      [:cyan]
   :footer      [:cyan]
   :warning-key [:bright]
   :focus-path  [:magenta]
   :message     [:magenta]
   :pointer     [:magenta]
   :none        [:reset]})

(defmacro black-and-white [body]
  (binding [*print-styles* {}]
    ~body))

(defn resolve-styles [styles]
  (if-let [res (not-empty
                  (mapcat #(or
                            (when-let [res (*print-styles* %)]
                              res)
                            [%])
                          styles))]
    res
    ;; fall back to bright
    [:bright]))

#_(resolve-styles [:highlight :green])

(defn color [s & styles]
  (apply ansi/style s (resolve-styles styles)))

(defmacro print-color-text [codes body]
  `(if ansi/*use-ansi*
     (do
       (.write ^java.io.Writer *out* (apply str (map ansi/ansi (resolve-styles ~codes))))
       ~body
       (.write ^java.io.Writer *out* (ansi/ansi :reset)))
     ~body))


