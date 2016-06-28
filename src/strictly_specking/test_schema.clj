(ns strictly-specking.test-schema
  (:require
   [clojure.spec :as s]
   [strictly-specking.core :refer [strict-keys def-key-doc]]))

(defmacro def-key [k spec doc]
  `(do
     (s/def ~k ~spec)
     (def-key-doc ~k ~doc)))

;; * Specification for ClojureScript
;; ** Top level util specs

(s/def ::string-or-symbol (s/or :string string? :symbol symbol?))
(s/def ::string-or-named  (s/or :string string? :symbol symbol? :keyword keyword?))

;; ** CLJS Compiler Options
;; *** Commonly used Compiler Options

(def-key :cljs-compiler/output-to string?
  "After your ClojureScript has been compiled to JavaScript, this
specifies the name of the JavaScript output file.  The contents of
this file will differ based on the :optimizations setting.

If :optimizations is set to :none then this file will merely contain
the code needed to load Google Closure the and rest of the compiled
namespaces (which are separate files).

If :optimizations is set to :simple, :whitespace, or :advanced this
output file will contain all the compiled code.

  :output-to \"resources/public/js/main.js\"")

(def-key :cljs-compiler/output-dir string?
  "Sets the output directory for output files generated during
compilation.

Defaults to  \"out\".

  :output-dir \"resources/public/js/out\"")

(def-key :cljs-compiler/optimizations #{:none :whitespace :simple :advanced}
"The optimization level. May be :none, :whitespace, :simple, or
:advanced. Only :none and :simple are supported for bootstrapped
ClojureScript.

  :none is the recommended setting for development
  
  :advanced is the recommended setting for production, unless
        something prevents it (incompatible external library, bug,
        etc.).

For a detailed explanation of the different optimization modes see
https://developers.google.com/closure/compiler/docs/compilation_levels

When the :main option is not used, :none requires manual code loading
and hence a separate HTML from the other options.

Defaults to :none. Note: lein cljsbuild 1.0.5 will supply :whitespace.

  :optimizations :none")

(def-key :cljs-compiler/main                      ::string-or-symbol
"Specifies an entry point namespace. When combined with optimization
level :none, :main will cause the compiler to emit a single JavaScript
file that will import goog/base.js, the JavaScript file for the
namespace, and emit the required goog.require statement. This permits
leaving HTML markup identical between dev and production.

Also see :asset-path.

  :main \"example.core\"")

(def-key :cljs-compiler/asset-path                string?
  "When using :main it is often necessary to control where the entry
point script attempts to load scripts from due to the configuration of
the web server. :asset-path is a relative URL path not a file system
path. For example, if your output directory is :ouput-dir
\"resources/public/js/compiled/out\" but your webserver is serving files
from \"resources/public\" then you want the entry point script to load
scripts from \"js/compiled/out\".

  :asset-path \"js/compiled/out\"")

(def-key :cljs-compiler/source-map                (s/or :bool boolean? :string string?)
  "See https://github.com/clojure/clojurescript/wiki/Source-maps. Under
optimizations :none the valid values are true and false, with the
default being true. Under all other optimization settings must specify
a path to where the source map will be written.

Under :simple, :whitespace, or :advanced
  :source-map \"path/to/source/map.js.map\"")


(s/def :cljs-compiler/preloads                  (s/+ symbol?)
  "Developing ClojureScript commonly requires development time only
side effects such as enabling printing, logging, spec instrumentation,
and connecting REPLs. :preloads permits loading such side effect
boilerplate right after cljs.core. For example you can make a
development namespace for enabling printing in browsers:

  (ns foo.dev)

  (enable-console-print!)

Now you can configure your development build to load this side effect
prior to your main namespace with the following compiler options:

  {:preloads [foo.dev]
   :main \"foo.core\"" "
   :output-dir \"out\"}

The :preloads config value must be a sequence of symbols that map to
existing namespaces discoverable on the classpath.")

(def-key :cljs-compiler/verbose                   boolean?
"Emit details and measurements from compiler activity.

  :verbose true")

(s/def :cljs-compiler/pretty-print              boolean?
  "Determines whether the JavaScript output will be tabulated in a
human-readable manner. Defaults to true.

  :pretty-print false")

(s/def :cljs-compiler/target                    #{:nodejs}
"If targeting nodejs add this line. Takes no other options at the
moment. The default (no :target specified) implies browsers are being
targeted. Have a look here for more information on how to run your
code in nodejs.

  :target :nodejs")

(s/def :cljs-compiler/foreign-libs
  (strict-keys
   :req-un [:cljs-foreign-libs/file
            :cljs-foreign-libs/provides]
   :opt-un [:cljs-foreign-libs/file-min
            :cljs-foreign-libs/requires
            :cljs-foreign-libs/module-type
            :cljs-foreign-libs/preprocess])
  "Adds dependencies on foreign libraries. Be sure that the url returns a
HTTP Code 200

Defaults to the empty vector []

  :foreign-libs [{ :file \"http://example.com/remote.js\"
                   :provides  [\"my.example\"]}]

Each element in the :foreign-libs vector should be a map, where the
keys have these semantics:

  :file Indicates the URL to the library

  :file-min (Optional) Indicates the URL to the minified variant of
            the library.
  
  :provides A synthetic namespace that is associated with the library.
            This is typically a vector with a single string, but it
            has the capability of specifying multiple namespaces
            (typically used only by Google Closure libraries).
  
  :requires (Optional) A vector explicitly identifying dependencies
            (:provides values from other foreign libs); used to form a
            topological sort honoring dependencies.

  :module-type (Optional) indicates that the foreign lib uses a given
               module system. Can be one of :commonjs, :amd, :es6.
               Note that if supplied, :requires is not used (as it is
               implicitly determined).
  
  :preprocess (Optional) Used to preprocess / transform code in other
              dialects (JSX, etc.). A defmethod for
              cljs.clojure/js-transforms must be provided that matches
              the supplied value in order to effect the desired code
              transformation.")

(s/def :cljs-foreign-libs/file        string?)
(s/def :cljs-foreign-libs/provides    (s/+ string?))
(s/def :cljs-foreign-libs/file-min    string?)
(s/def :cljs-foreign-libs/requires    (s/+ string?))
(s/def :cljs-foreign-libs/module-type #{:commonjs :amd :es6})
(s/def :cljs-foreign-libs/preprocess  ::string-or-named)

(s/def :cljs-compiler/externs                    (s/+ string?)
"Configure externs files for external libraries.

For this option, and those below, you can find a very good explanation at: http://lukevanderhart.com/2011/09/30/using-javascript-and-clojurescript.html

Defaults to the empty vector [].

  :externs [\"jquery-externs.js\"]")

(s/def :cljs-compiler/modules
  (s/map-of
   keyword?
   (strict-keys
    :req-un [:cljs-compiler-modules/output-dir
             :cljs-compiler-modules/entries]
    :opt-un [:cljs-compiler-modules/depends-on]))
"A new option for emitting Google Closure Modules. Closure Modules
supports splitting up an optimized build into N different modules. If
:modules is supplied it replaces the single :output-to. A module needs
a name, an individual :output-to file path, :entries a set of
namespaces, and :depends-on a set of modules on which the module
depends. Modules are only supported with :simple and :advanced
optimizations. An example follows:

  {:optimizations :advanced
   :source-map true
   :output-dir \"resources/public/js\"
   :modules {
     :common 
       {:output-to \"resources/public/js/common.js\"  
        :entries #{\"com.foo.common\"}}
     :landing 
       {:output-to \"resources/public/js/landing.js\" 
        :entries #{\"com.foo.landing\"}
        :depends-on #{:common}}
     :editor 
       {:output-to \"resources/public/js/editor.js\"
        :entries #{\"com.foo.editor\"}
        :depends-on #{:common}}}}


Any namespaces not in an :entries set will be moved into the default
module :cljs-base. However thanks to cross module code motion, Google
Closure can move functions and methods into the modules where they are
actually used. This process is somewhat conservative so if you know
that you want to keep some code together do this via :entries.

The :cljs-base module defaults to being written out to :output-dir
with the name \"cljs_base.js\". This may be overridden by specifying a
:cljs-base module describing only :output-to.

Take careful note that a namespace may only appear once across all
module :entries.

:modules fully supports :foreign-libs. :foreign-libs are always put
into dependency order before any Google Closure compiled source.

Source maps are fully supported, an individual one will be created for
each module. Just supply :source-map true (see example) as there is no
single source map to name.")

(s/def :cljs-compiler-modules/output-dir    string?)
(s/def :cljs-compiler-modules/entries       (s/+ string?))
(s/def :cljs-compiler-modules/depends-on    (s/+ ::string-or-named))

(s/def :cljs-compiler/source-map-path            string?
"Set the path to source files references in source maps to avoid
further web server configuration.

  :source-map-path \"public/js\"")

(s/def :cljs-compiler/source-map-timestamp       boolean? 
"Add cache busting timestamps to source map urls. This is helpful for
keeping source maps up to date when live reloading code.

  :source-map-timestamp true")

(s/def :cljs-compiler/cache-analysis             boolean?
"Experimental. Cache compiler analysis to disk. This enables faster
cold build and REPL start up times.

For REPLs, defaults to true. Otherwise, defaults to true if and only
if :optimizations is :none.

  :cache-analysis true")

(s/def :cljs-compiler/recompile-dependents       boolean?
"For correctness the ClojureScript compiler now always recompiles
dependent namespaces when a parent namespace changes. This prevents
corrupted builds and swallowed warnings. However this can impact
compile times depending on the structure of the application. This
option defaults to true.

  :recompile-dependents false")

(s/def :cljs-compiler/static-fns                 boolean?
"Employs static dispatch to specific function arities in emitted
JavaScript, as opposed to making use of the call construct. Defaults
to false except under advanced optimizations. Useful to have set to
false at REPL development to facilitate function redefinition, and
useful to set to true for release for performance.

This setting does not apply to the standard library, which is always
compiled with :static-fns implicitly set to true.

  :static-fns true")

;; (s/def :cljs-compiler/warnings                   (ref-schema 'CompilerWarnings))

(s/def :cljs-compiler/elide-asserts              boolean?
  "This flag will cause all (assert x) calls to be removed during
compilation, including implicit asserts associated with :pre and :post
conditions. Useful for production. Default is always false even in
advanced compilation. Does NOT specify goog.asserts.ENABLE_ASSERTS,
which is different and used by the Closure library.

Note that it is currently not possible to dynamically set *assert* to
false at runtime; this compiler flag must explicitly be used to effect
the elision.

  :elide-asserts true")

(s/def :cljs-compiler/pseudo-names               boolean?
  "With :advanced mode optimizations, determines whether readable names
are emitted. This can be useful when debugging issues in the optimized
JavaScript and can aid in finding missing externs. Defaults to false.

  :pseudo-names true")

(s/def :cljs-compiler/print-input-delimiter      boolean?
 "Determines whether comments will be output in the JavaScript that can
be used to determine the original source of the compiled code.

Defaults to false.

  :print-input-delimiter false")

(s/def :cljs-compiler/output-wrapper             boolean?
  "Wrap the JavaScript output in (function(){...};)() to avoid clobbering
globals. Defaults to false.

  :output-wrapper false")

(s/def :cljs-compiler/libs                       (s/+ string?)
  "Adds dependencies on external js libraries, i.e. Google
Closure-compatible javascript files with correct goog.provides() and
goog.requires() calls. Note that files in these directories will be
watched and a rebuild will occur if they are modified.

Paths or filenames can be given. Relative paths are relative to the
current working directory (usually project root).

Defaults to the empty vector []

  :libs [\"closure/library/third_party/closure\"
         \"src/js\"
         \"src/org/example/example.js\"]")

(s/def :cljs-compiler/preamble                   (s/+ string?)
"Prepends the contents of the given files to each output file. Only
valid with optimizations other than :none.

Defaults to the empty vector []

  :preamble [\"license.js\"]")

(s/def :cljs-compiler/hashbang                   boolean?
"When using :target :nodejs the compiler will emit a shebang as the
first line of the compiled source, making it executable. When your
intention is to build a node.js module, instead of executable, use
this option to remove the shebang.

  :hashbang false")

(s/def :cljs-compiler/compiler-stats             boolean?
"Report basic timing measurements on compiler activity.

Defaults to false.

  :compiler-stats true")

(s/def :cljs-compiler/language-in                #{:ecmascript3 :ecmascript5 :ecmascript5-strict}
"Configure the input and output languages for the closure library. May
be :ecmascript3, ecmascript5, ecmascript5-strict, :ecmascript6-typed,
:ecmascript6-strict, :ecmascript6 or :no-transpile.

Defaults to :ecmascript3

  :language-in  :ecmascript3")


(s/def :cljs-compiler/language-out               #{:ecmascript3 :ecmascript5 :ecmascript5-strict}
"Configure the input and output languages for the closure library. May
be :ecmascript3, ecmascript5, ecmascript5-strict, :ecmascript6-typed,
:ecmascript6-strict, :ecmascript6 or :no-transpile.

Defaults to :ecmascript3

  :language-out  :ecmascript3")

(s/def :cljs-compiler/closure-defines
  (s/map-of
   ::string-or-symbol
   (s/or :number number? :string string? :bool   boolean?))
  "Set the values of Closure libraries' variables annotated with @define
or with the cljs.core/goog-define helper macro. A common usage is
setting goog.DEBUG to false:

  :closure-defines {\"goog.DEBUG\" false}

or

  :closure-defines {'goog.DEBUG false}

Note when using Lein the quote is unnecessary due to implicit quoting.

For :optimization :none, a :main option must be specified for defines
to work, and only goog-define defines are affected. :closure-defines
currently does not have any effect with :optimization :whitespace.")

(s/def :cljs-compiler/closure-extra-annotations  (s/+ string?)
"Define extra JSDoc annotations that a closure library might use so
that they don't trigger compiler warnings.

  :closure-extra-annotations #{\"api\"}")

(s/def :cljs-compiler/anon-fn-naming-policy      #{:off :unmapped :mapped}
"Strategies for how the Google Closure compiler does naming of
anonymous functions that occur as r-values in assignments and variable
declarations. Defaults to :off.

  :anon-fn-naming-policy :unmapped

The following values are supported:

  :off Don't give anonymous functions names.
  
  :unmapped Generates names that are based on the left-hand side of
            the assignment. Runs after variable and property renaming,
            so that the generated names will be short and obfuscated.
  
  :mapped Generates short unique names and provides a mapping from
          them back to a more meaningful name that's based on the
          left-hand side of the assignment.")

(s/def :cljs-compiler/optimize-constants         boolean?
"When set to true, constants, such as keywords and symbols, will only
be created once and will be written to a separate file called
constants_table.js. The compiler will emit a reference to the constant
as defined in the constants table instead of creating a new object for
it. This option is mainly intended to be used for a release build
since it can increase performance due to decreased allocation.
Defaults to true under :advanced optimizations otherwise to false.

  :optimize-constants true")

(s/def :cljs-compiler/parallel-build             boolean?
  "When set to true, compile source in parallel, utilizing multiple cores.

:parallel-build true")

(s/def :cljs-compiler/devcards                   boolean?
"Whether to include devcard 'defcard' definitions in the output of the compile.")

(s/def :cljs-compiler/dump-core                  boolean?)
(s/def :cljs-compiler/emit-constants             boolean?)
(s/def :cljs-compiler/warning-handlers           (s/+ ::s/any)) ;; symbol, string, or fn?
(s/def :cljs-compiler/source-map-inline          boolean?)
(s/def :cljs-compiler/ups-libs                   (s/+ string?))
(s/def :cljs-compiler/ups-externs                (s/+ string?))
(s/def :cljs-compiler/ups-foreign-libs           (s/+ :cljs-compiler/foreign-libs))
(s/def :cljs-compiler/closure-output-charset     string?)
(s/def :cljs-compiler/external-config            (s/map-of keyword? map?))

;; ** ClojureScript Compiler Warnings

(s/def :cljs-compiler/warnings
  (strict-keys
   :opt-un
   [:cljs-compiler-warning/undeclared-ns-form
    :cljs-compiler-warning/protocol-deprecated
    :cljs-compiler-warning/undeclared-protocol-symbol
    :cljs-compiler-warning/fn-var
    :cljs-compiler-warning/invalid-arithmetic
    :cljs-compiler-warning/preamble-missing
    :cljs-compiler-warning/undeclared-var
    :cljs-compiler-warning/protocol-invalid-method
    :cljs-compiler-warning/variadic-max-arity
    :cljs-compiler-warning/multiple-variadic-overloads
    :cljs-compiler-warning/fn-deprecated
    :cljs-compiler-warning/redef
    :cljs-compiler-warning/fn-arity
    :cljs-compiler-warning/invalid-protocol-symbol
    :cljs-compiler-warning/dynamic
    :cljs-compiler-warning/undeclared-ns
    :cljs-compiler-warning/overload-arity
    :cljs-compiler-warning/extending-base-js-type
    :cljs-compiler-warning/single-segment-namespace
    :cljs-compiler-warning/protocol-duped-method
    :cljs-compiler-warning/protocol-multiple-impls
    :cljs-compiler-warning/invoke-ctor])
  "This flag will turn on/off compiler warnings for references to
undeclared vars, wrong function call arities, etc. Can be a boolean
for enabling/disabling common warnings, or a map of specific warning
keys with associated booleans. Defaults to true.

  :warnings true
  
;; OR

  :warnings {:fn-deprecated false} ;; suppress this warning

The following warnings are supported:

  :preamble-missing, missing preamble
  :undeclared-var, undeclared var
  :undeclared-ns, var references non-existent namespace
  :undeclared-ns-form, namespace reference in ns form that does not exist
  :redef, var redefinition
  :dynamic, dynamic binding of non-dynamic var
  :fn-var, var previously bound to fn changed to different type
  :fn-arity, invalid invoke arity
  :fn-deprecated, deprecated function usage
  :protocol-deprecated, deprecated protocol usage
  :undeclared-protocol-symbol, undeclared protocol referred
  :invalid-protocol-symbol, invalid protocol symbol
  :multiple-variadic-overloads, multiple variadic arities
  :variadic-max-arity, arity greater than variadic arity
  :overload-arity, duplicate arities
  :extending-base-js-type, JavaScript base type extension
  :invoke-ctor, type constructor invoked as function
  :invalid-arithmetic, invalid arithmetic
  :protocol-invalid-method, protocol method does not match declaration
  :protocol-duped-method, duplicate protocol method implementation
  :protocol-multiple-impls, protocol implemented multiple times
  :single-segment-namespace, single segment namespace")

(s/def :cljs-compiler-warning/undeclared-ns-form          boolean?)
(s/def :cljs-compiler-warning/protocol-deprecated         boolean?)
(s/def :cljs-compiler-warning/undeclared-protocol-symbol  boolean?)
(s/def :cljs-compiler-warning/fn-var                      boolean?)
(s/def :cljs-compiler-warning/invalid-arithmetic          boolean?)
(s/def :cljs-compiler-warning/preamble-missing            boolean?)
(s/def :cljs-compiler-warning/undeclared-var              boolean?)
(s/def :cljs-compiler-warning/protocol-invalid-method     boolean?)
(s/def :cljs-compiler-warning/variadic-max-arity          boolean?)
(s/def :cljs-compiler-warning/multiple-variadic-overloads boolean?)
(s/def :cljs-compiler-warning/fn-deprecated               boolean?)
(s/def :cljs-compiler-warning/redef                       boolean?)
(s/def :cljs-compiler-warning/fn-arity                    boolean?)
(s/def :cljs-compiler-warning/invalid-protocol-symbol     boolean?)
(s/def :cljs-compiler-warning/dynamic                     boolean?)
(s/def :cljs-compiler-warning/undeclared-ns               boolean?)
(s/def :cljs-compiler-warning/overload-arity              boolean?)
(s/def :cljs-compiler-warning/extending-base-js-type      boolean?)
(s/def :cljs-compiler-warning/single-segment-namespace    boolean?)
(s/def :cljs-compiler-warning/protocol-duped-method       boolean?)
(s/def :cljs-compiler-warning/protocol-multiple-impls     boolean?)
(s/def :cljs-compiler-warning/invoke-ctor                 boolean?)

;; ** Closure Compiler Warnings

(s/def :cljs-compiler/closure-warnings
  (strict-keys
   :opt-un
   [:closure-warning/access-controls
    :closure-warning/ambiguous-function-decl
    :closure-warning/debugger-statement-present
    :closure-warning/check-regexp
    :closure-warning/check-types
    :closure-warning/check-useless-code
    :closure-warning/check-variables
    :closure-warning/const
    :closure-warning/constant-property
    :closure-warning/deprecated
    :closure-warning/duplicate-message
    :closure-warning/es5-strict
    :closure-warning/externs-validation
    :closure-warning/fileoverview-jsdoc
    :closure-warning/global-this
    :closure-warning/internet-explorer-checks
    :closure-warning/invalid-casts
    :closure-warning/missing-properties
    :closure-warning/non-standard-jsdoc
    :closure-warning/strict-module-dep-check
    :closure-warning/tweaks
    :closure-warning/undefined-names
    :closure-warning/undefined-variables
    :closure-warning/unknown-defines
    :closure-warning/visiblity])
  "Configure warnings generated by the Closure compiler. A map from
Closure warning to configuration value, only :error, :warning and :off
are supported.

  :closure-warnings {:externs-validation :off}

The following Closure warning options are exposed to ClojureScript:

  :access-controls
  :ambiguous-function-decl
  :debugger-statement-present
  :check-regexp
  :check-types
  :check-useless-code
  :check-variables
  :const
  :constant-property
  :deprecated
  :duplicate-message
  :es5-strict
  :externs-validation
  :fileoverview-jsdoc
  :global-this
  :internet-explorer-checks
  :invalid-casts
  :missing-properties
  :non-standard-jsdoc
  :strict-module-dep-check
  :tweaks
  :undefined-names
  :undefined-variables
  :unknown-defines
  :visiblity
  
See the Closure Compiler Warning wiki for detailed descriptions.")

(s/def :closure-warning/value #{:error :warning :off})

(s/def :closure-warning/access-controls            :closure-warning/value)
(s/def :closure-warning/ambiguous-function-decl    :closure-warning/value)
(s/def :closure-warning/debugger-statement-present :closure-warning/value)
(s/def :closure-warning/check-regexp               :closure-warning/value)
(s/def :closure-warning/check-types                :closure-warning/value)
(s/def :closure-warning/check-useless-code         :closure-warning/value)
(s/def :closure-warning/check-variables            :closure-warning/value)
(s/def :closure-warning/const                      :closure-warning/value)
(s/def :closure-warning/constant-property          :closure-warning/value)
(s/def :closure-warning/deprecated                 :closure-warning/value)
(s/def :closure-warning/duplicate-message          :closure-warning/value)
(s/def :closure-warning/es5-strict                 :closure-warning/value)
(s/def :closure-warning/externs-validation         :closure-warning/value)
(s/def :closure-warning/fileoverview-jsdoc         :closure-warning/value)
(s/def :closure-warning/global-this                :closure-warning/value)
(s/def :closure-warning/internet-explorer-checks   :closure-warning/value)
(s/def :closure-warning/invalid-casts              :closure-warning/value)
(s/def :closure-warning/missing-properties         :closure-warning/value)
(s/def :closure-warning/non-standard-jsdoc         :closure-warning/value)
(s/def :closure-warning/strict-module-dep-check    :closure-warning/value)
(s/def :closure-warning/tweaks                     :closure-warning/value)
(s/def :closure-warning/undefined-names            :closure-warning/value)
(s/def :closure-warning/undefined-variables        :closure-warning/value)
(s/def :closure-warning/unknown-defines            :closure-warning/value)
(s/def :closure-warning/visiblity                  :closure-warning/value)



;; ** Figwheel Client Options

(s/def :figwheel-client-options/build-id             string?)
(s/def :figwheel-client-options/websocket-host       (s/or :string string?
                                                           :host-option
                                                           #{:js-client-host :server-ip :server-hostname}))
(s/def :figwheel-client-options/websocket-url        string?)
(s/def :figwheel-client-options/on-jsload            ::string-or-named)
(s/def :figwheel-client-options/before-jsload        ::string-or-named)
(s/def :figwheel-client-options/on-cssload           ::string-or-named)
(s/def :figwheel-client-options/on-message           ::string-or-named)
(s/def :figwheel-client-options/on-compile-fail      ::string-or-named)
(s/def :figwheel-client-options/on-compile-warning   ::string-or-named)
(s/def :figwheel-client-options/reload-dependents    boolean?)
(s/def :figwheel-client-options/debug                boolean?)
(s/def :figwheel-client-options/autoload             boolean?)
(s/def :figwheel-client-options/heads-up-display     boolean?)
(s/def :figwheel-client-options/load-warninged-code  boolean?)
(s/def :figwheel-client-options/retry-count          integer?)
(s/def :figwheel-client-options/devcards             boolean?)
(s/def :figwheel-client-options/eval-fn              ::string-or-named)
(s/def :figwheel-client-options/open-urls            (s/+ string?))

;; ** Figwheel Build Config

(s/def :figwheel-build-config/id               ::string-or-named)
(s/def :figwheel-build-config/source-paths     (s/+ string?))
(s/def :figwheel-build-config/notify-command   (s/+ string?))
(s/def :figwheel-build-config/jar              boolean?)
(s/def :figwheel-build-config/incremental      boolean?)
(s/def :figwheel-build-config/assert           boolean?)
(s/def :figwheel-build-config/warning-handlers (s/+ ::s/any))

(s/def :figwheel-build-config/figwheel
  (s/or
   :bool boolean?
   :figwheel-client-options
   (strict-keys
    :opt-un
    [:figwheel-client-options/build-id
     :figwheel-client-options/websocket-host
     :figwheel-client-options/websocket-url
     :figwheel-client-options/on-jsload
     :figwheel-client-options/before-jsload
     :figwheel-client-options/on-cssload
     :figwheel-client-options/on-message
     :figwheel-client-options/on-compile-fail
     :figwheel-client-options/on-compile-warning
     :figwheel-client-options/reload-dependents
     :figwheel-client-options/debug
     :figwheel-client-options/autoload
     :figwheel-client-options/heads-up-display
     :figwheel-client-options/load-warninged-code
     :figwheel-client-options/retry-count
     :figwheel-client-options/devcards
     :figwheel-client-options/eval-fn
     :figwheel-client-options/open-urls])))

(s/def :figwheel-build-config/compiler
  (strict-keys
   :opt-un
   [:cljs-compiler/main
    :cljs-compiler/preloads
    :cljs-compiler/asset-path
    :cljs-compiler-options/output-to
    :cljs-compiler/output-dir
    :cljs-compiler/optimizations
    :cljs-compiler/source-map
    :cljs-compiler/verbose
    :cljs-compiler/pretty-print
    :cljs-compiler/target
    :cljs-compiler/foreign-libs
    :cljs-compiler/externs
    :cljs-compiler/modules
    :cljs-compiler/source-map-path
    :cljs-compiler/source-map-timestamp
    :cljs-compiler/cache-analysis
    :cljs-compiler/recompile-dependents
    :cljs-compiler/static-fns
    :cljs-compiler/elide-asserts
    :cljs-compiler/pseudo-names
    :cljs-compiler/print-input-delimiter
    :cljs-compiler/output-wrapper
    :cljs-compiler/libs
    :cljs-compiler/preamble
    :cljs-compiler/hashbang
    :cljs-compiler/compiler-stats
    :cljs-compiler/language-in
    :cljs-compiler/language-out
    :cljs-compiler/closure-defines
    :cljs-compiler/closure-extra-annotations
    :cljs-compiler/anon-fn-naming-policy
    :cljs-compiler/optimize-constants
    :cljs-compiler/parallel-build
    :cljs-compiler/devcards
    :cljs-compiler/dump-core
    :cljs-compiler/emit-constants
    :cljs-compiler/warning-handlers
    :cljs-compiler/source-map-inline
    :cljs-compiler/ups-libs
    :cljs-compiler/ups-externs
    :cljs-compiler/ups-foreign-libs
    :cljs-compiler/closure-output-charset
    :cljs-compiler/external-config]))

(s/def :figwheel-build-config/options
  (strict-keys
   :opt-un
   [:figwheel-build-config/id
    :figwheel-build-config/notify-command
    :figwheel-build-config/jar
    :figwheel-build-config/incremental
    :figwheel-build-config/assert
    :figwheel-build-config/warning-handlers
    :figwheel-build-config/figwheel]
   :req-un
   [:figwheel-build-config/source-paths
    :figwheel-build-config/compiler]))

(s/def :figwheel-build-config/compiler-require-id
  (s/and
   :figwheel-build-config/options
   (s/keys
    :req-un [:figwheel-build-config/id])))

(s/def :figwheel-options/http-server-root  string?)
(s/def :figwheel-options/server-port       integer?)
(s/def :figwheel-options/server-ip         string?)
(s/def :figwheel-options/css-dirs          (s/+ string?))
(s/def :figwheel-options/ring-handler      ::string-or-named)
(s/def :figwheel-options/builds-to-start   (s/+ ::string-or-named))
(s/def :figwheel-options/server-logfile    string?)
(s/def :figwheel-options/open-file-command string?)
(s/def :figwheel-options/repl              boolean?)
(s/def :figwheel-options/nrepl-port        integer?)
(s/def :figwheel-options/nrepl-host        string?)
(s/def :figwheel-options/nrepl-middleware  (s/+ ::string-or-named)) ;; allow inlined
(s/def :figwheel-options/validate-config   boolean?)
(s/def :figwheel-options/load-all-builds   boolean?)
(s/def :figwheel-options/ansi-color-output boolean?)

(s/def :figwheel-options/builds
  (s/or
   :builds-vector (s/+ :figwheel-build-config/compiler-require-id)
   :builds-map    (s/map-of ::string-or-named :figwheel-build-config/options)))

(s/def :figwheel-options/hawk-options
  (s/map-of #{:watcher} #{:barbary :java :polling}))

(s/def :figwheel-options-reload-clj-files/clj  boolean?)
(s/def :figwheel-options-reload-clj-files/cljs boolean?)

(s/def :figwheel-options/reload-clj-files
  (s/or
   :bool boolean?
   :suffix-map
   (strict-keys
    :opt-un
    [:figwheel-options-reload-clj-files/clj
     :figwheel-options-reload-clj-files/cljs])))

;; ** Lein Cljsbuild Options

#_(s/def :cljsbuild-options/builds                (ref-schema 'CljsBuilds))
(s/def :cljsbuild-options/repl-listen-port      integer?)
(s/def :cljsbuild-options/repl-launch-commands  (map-of
                                                 ::string-or-named
                                                 (s/+ ::string-or-named)))
(s/def :cljsbuild-options/test-commands         (map-of
                                                 ::string-or-named
                                                 (s/+ ::string-or-named)))
(s/def :cljsbuild-options/crossovers            (s/+ ::s/any))
(s/def :cljsbuild-options/crossover-path        (s/+ ::s/any))
(s/def :cljsbuild-options/crossover-jar         boolean?)

(s/def :cljsbuild-options/cljsbuilds
  (strict-keys
   :opt-un [:figwheel-options/builds
            :cljsbuild-options/repl-listen-port
            :cljsbuild-options/repl-launch-commands
            :cljsbuild-options/test-commands
            :cljsbuild-options/crossovers
            :cljsbuild-options/crossover-path
            :cljsbuild-options/crossover-jar]))


;; ** Different Top Level Schemas depending on context

