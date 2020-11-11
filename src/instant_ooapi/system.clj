(ns instant-ooapi.system
  (:require
    [aero.core :as aero]
    [clojure.java.io :as io]
    [integrant.core :as ig]))

(defmethod aero/reader 'ig/ref
  [_ _tag value]
  (ig/ref value))

(defn config
  [profile]
  (aero/read-config (io/resource "system.edn") {:profile profile}))

(defn prep
  [profile]
  (let [config (config profile)]
    (ig/load-namespaces config)
    config))
