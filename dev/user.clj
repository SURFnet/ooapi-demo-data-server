(ns user
  (:require [nl.jomco.resources :refer [defresource close]]
            [ooapi-demo-data-server.system :as system]
            [clojure.tools.logging :as log]
            [nl.surf.eduhub-validator.main :as validator]))

;; TODO: Why does this not print to the REPL in CIDER?

(log/info "User namespace Loaded. use (start!) to run service")

(defresource system)

(defn start!
  []
  (defresource system (system/system)))

(defn stop!
  []
  (close system))

(defn validate!
  [profile]
  (validator/-main "-u" "http://localhost:8080" "-m" "1" "-r" profile))
