(ns instant-ooapi.http
  (:require
    [org.httpkit.server :as http]
    [integrant.core :as ig]))

(defmethod ig/init-key ::server
  [_ {:keys [handler settings]}]
  (http/run-server handler settings))

(defmethod ig/halt-key! ::server
  [_ server]
  (server :timeout 100))

(comment

  (user/go)

  nil)
