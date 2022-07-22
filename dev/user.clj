(ns user)

(defmacro jit
  "Just in time loading of dependencies."
  [sym]
  `(requiring-resolve '~sym))

(defn set-prep!
  []
  ((jit integrant.repl/set-prep!) #((jit ooapi-demo-data-server.system/prep) :dev)))

(defn go
  []
  (set-prep!)
  ((jit integrant.repl/go)))

(defn reset
  []
  (set-prep!)
  ((jit integrant.repl/reset)))

(defn system
  []
  @(jit integrant.repl.state/system))

(defn config
  []
  @(jit integrant.repl.state/config))
