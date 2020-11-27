(ns instant-ooapi.data
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [nl.surf.demo-data.config :as config]
    [nl.surf.demo-data.world :as world]))

(defmethod config/generator "minus" [_]
  (fn minus [_ & xs]
    (apply - xs)))

(defmethod config/generator "sanitize" [_]
  (fn sanitize [_ x]
    (when x
      (-> x
          str/lower-case
          str/trim
          (str/replace #"[^a-z0-9]+" "-")
          (str (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*))))))

(def data
  (-> "resources/schema.edn"
      (slurp)
      (edn/read-string)
      (json/generate-string)
      (json/decode true)
      (config/load)
      (world/gen (-> "resources/pop.edn"
                     (slurp)
                     (edn/read-string)))))

(def schema (json/parse-string (slurp "resources/ooapiv4.json")
                               #(if (str/starts-with? % "/") % (keyword %))))

(def route-data
  {"/courses/{courseId}" {:ooapi/cardinality :one
                          :ooapi/datatype :course
                          :ooapi/id-path [:parameters :path :courseId]}
   "/courses"            {:ooapi/cardinality :many
                          :ooapi/datatype :course
                          :ooapi/filters #{:level :modeOfDelivery}
                          :ooapi/q-fields #{:name :abbreviation :description}
                          :ooapi/sort #{"name" "ects" "courseId"}}})


(defn build-routes
  [schema]
  (for [[path methods] (:paths schema)]
    (let [{:keys [description summary parameters]} (:get methods)]
      (merge {:description description
              :name summary
              :parameters parameters
              :path path}
             (get route-data path {})))))

(defn routes
  []
  (->> (build-routes schema)
       (filter (comp (-> route-data keys set) :path)))) ; filter routes to only the ones we have route data for

(comment
  (->> schema
       :paths
       vals
       (map :get)
       (mapcat :parameters)
       (distinct)
       (map :schema)
       (filter (comp #{"array"} :type))
       (distinct))

  (->> (routes)
       first
       :parameters)

  nil)