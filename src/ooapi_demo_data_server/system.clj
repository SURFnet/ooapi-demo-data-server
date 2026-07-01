(ns ooapi-demo-data-server.system
  (:require
    [aero.core :as aero]
    [clojure.java.io :as io]
    [nl.jomco.resources :refer [mk-system]]
    [ooapi-demo-data-server.data :as data]
    [ooapi-demo-data-server.handler :as handler]
    [ooapi-demo-data-server.http :as http]))

(defn system-config
  []
  (let [cfg (aero/read-config (io/resource "config.edn"))]
    (update-in cfg [:data :seed] #(or % (rand-int 10000)))))

(defn system
  ([system-config]
   (mk-system [data (data/generate-data (:data system-config))
               routes (data/routes (:data system-config))
               app (handler/app data routes (:app system-config))
               _server (http/server app (:http system-config))]))
  ([]
   (system (system-config))))
