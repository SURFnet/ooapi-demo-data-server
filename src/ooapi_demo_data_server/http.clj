(ns ooapi-demo-data-server.http
  (:require
    [org.httpkit.server :as http]
    [nl.jomco.resources :refer [closeable]]))

(defn server
  [handler settings]
  ;; implement `nl.jomco.resources/close` on the http-kit server
  (let [callback (http/run-server handler settings)]
    (closeable callback (fn [_]
                          (callback :timeout 100)))))
