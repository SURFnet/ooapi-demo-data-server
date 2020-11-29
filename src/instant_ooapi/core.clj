(ns instant-ooapi.core
  (:require
    [instant-ooapi.system :as system]
    [io.pedestal.log :as log]
    [integrant.core :as ig]))

(defn -main
  [& args]
  (log/info :instant-ooapi/starting {})
  (let [system (ig/init (system/prep :prod))]
    system))
