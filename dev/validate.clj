(ns validate
  (:require
    [instant-ooapi.data :as data]))

; Load credentials for gateway
(def user (System/getenv "GATEWAY_USER"))
(def password (System/getenv "GATEWAY_PASSWORD"))
