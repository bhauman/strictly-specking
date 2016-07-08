(ns strictly-specking.ansi-util
  (:require
   [clojure.string :as str]))

#_ (remove-ns 'strictly-specking.ansi-util)

;; this is stolen from puget as up to date ansi lib is not really available
;; https://github.com/greglook/puget/blob/develop/src/puget/color/ansi.clj

(def sgr-code
  "Map of symbols to numeric SGR (select graphic rendition) codes."
  {:none        0
   :bold        1
   :underline   3
   :blink       5
   :reverse     7
   :hidden      8
   :strike      9
   :black      30
   :red        31
   :green      32
   :yellow     33
   :blue       34
   :magenta    35
   :cyan       36
   :white      37
   :fg-256     38
   :fg-reset   39
   :bg-black   40
   :bg-red     41
   :bg-green   42
   :bg-yellow  43
   :bg-blue    44
   :bg-magenta 45
   :bg-cyan    46
   :bg-white   47
   :bg-256     48
   :bg-reset   49})

(def ^:dynamic *enable-color* false)

(defn esc
  "Returns an ANSI escope string which will apply the given collection of SGR
  codes."
  [codes]
  (let [codes (map sgr-code codes codes)
        codes (str/join \; codes)]
    (str \u001b \[ codes \m)))

(defn escape
  "Returns an ANSI escope string which will enact the given SGR codes."
  [& codes]
  (esc codes))

(defn sgr
  "Wraps the given string with SGR escapes to apply the given codes, then reset
  the graphics."
  [string & codes]
  (str (esc codes) string (escape :none)))

(defn strip
  "Removes color codes from the given string."
  [string]
  (str/replace string #"\u001b\[[0-9;]*[mK]" ""))

(defmacro with-color [& body]
  `(binding [*enable-color* true]
     ~@body))

(defmacro with-color-when [b & body]
  `(if ~b
     (with-color ~@body)
     (do ~@body)))

(def ansi-code? sgr-code)

(def ^:dynamic *print-styles*
  {:highlight   [:bold]
   :good        [:green]
   :good-pred   [:green]
   :good-key    [:green]      
   :bad         [:red]
   :bad-value   [:red]
   :error-key   [:red]
   :focus-key   [:bold]   
   :correct-key [:green]   
   :header      [:cyan]
   :footer      [:cyan]
   :warning-key [:bold]
   :focus-path  [:magenta]
   :message     [:magenta]
   :pointer     [:magenta]
   :none        [:none]})

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
    [:bold]))

#_(resolve-styles [:highlight :green])

(defn color [s & styles]
  (apply sgr s (resolve-styles styles)))

#_(println (with-color (color "hi" :cyan )))

(def color-text color)

(defmacro print-color-text
  [codes body]
  `(if *enable-color*
     (do
       (.write ^java.io.Writer *out* (apply str (esc (resolve-styles ~codes))))
       ~body
       (.write ^java.io.Writer *out* (escape :none)))
     ~body))

#_(with-color (print-color-text [:green]
                                (print "ouch")))
