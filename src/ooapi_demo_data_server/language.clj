(ns ooapi-demo-data-server.language
  (:require
   [clojure.java.io :as io])
  (:import
   [opennlp.tools.langdetect LanguageDetectorModel LanguageDetectorME]))

(def model-input-stream (-> "langdetect-183.bin"
                            (io/resource)
                            (io/input-stream)))

(def model (LanguageDetectorModel. model-input-stream))

(def detector (LanguageDetectorME. model))


(defn detect
  "Attempts to detect language of string `s`"
  [s]
  (when s
    (let [prediction (.predictLanguage detector s)]
      (.getLang prediction))))
