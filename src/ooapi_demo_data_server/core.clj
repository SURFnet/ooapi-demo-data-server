(ns ooapi-demo-data-server.core
  (:require
    [ooapi-demo-data-server.system :as system]
    [clojure.tools.logging :as log]
    [integrant.core :as ig]
    [clojure.data.generators])
  (:gen-class))

(defn seed
  []
  (let [e (System/getenv "SEED")
        s (try (Integer/parseInt e)
            (catch Exception _ (rand-int 100000)))]
    (or s (rand 100000))))

(defn -main
  [& _]
  (log/info :ooapi-demo-data-server/starting {})
  (let [s (seed)
        system (binding [clojure.data.generators/*rnd* (java.util.Random. s)]
                 (ig/init (system/prep :prod)))]
    (log/info :ooapi-demo-data-server/seed {:seed s})
    system))
