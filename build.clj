(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(def lib 'nl.surf/ooapi-demo-data-server)

;; if you want a version of MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))

(defn uberjar
  "Run the CI pipeline of tests (and build the uberjar)."
  [opts]
  (-> opts
      (assoc :lib lib
             :version version
             :main 'ooapi-demo-data-server.core
             :uber-file "target/ooapi-demo-data-server.jar"
             :src-dirs ["src"]
             :resource-dirs ["resources"])
      (bb/clean)
      (bb/uber)))