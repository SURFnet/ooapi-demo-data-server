(ns user
  (:require [nl.jomco.resources :refer [defresource close]]
            [ooapi-demo-data-server.system :as system]
            [clojure.tools.logging :as log]))

;; TODO: Why does this not print to the REPL in CIDER?

(log/info "User namespace Loaded. use (start!) to run service")

(defresource system)

(defn start!
  []
  (defresource system (system/system)))

(defn stop!
  []
  (close system))

