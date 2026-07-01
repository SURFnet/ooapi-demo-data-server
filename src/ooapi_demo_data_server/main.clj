(ns ooapi-demo-data-server.main
  (:require
    [ooapi-demo-data-server.system :as system]
    [clojure.tools.logging :as log]
    [clojure.data.generators]
    [nl.jomco.resources :refer [with-resources wait-until-interrupted]])
  (:gen-class))

(defn main
  []
  (log/info :ooapi-demo-data-server/starting {})
  (let [config (system/system-config)]
    (log/info :ooapi-demo-data-server/seed (get-in config [:app :seed]))
    (with-resources [_sys (system/system config)]
      (wait-until-interrupted))))

(defn -main
  [& _]
  (main))
