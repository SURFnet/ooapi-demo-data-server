(ns instant-ooapi.data
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [nl.surf.demo-data.config :as config]
    [nl.surf.demo-data.world :as world]
    [clojure.java.io :as io]))

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

; TODO fix keywordizing. Probably need to decode without keywordizing and keywordize manually using clojure.walk.
(defn generate-data
  []
  (-> "schema.edn"
      (io/resource)
      (slurp)
      (edn/read-string)
      (json/generate-string)
      (json/decode true)
      (config/load)
      (world/gen (-> "pop.edn"
                     (io/resource)
                     (slurp)
                     (edn/read-string)))))

(def data (generate-data))

(def schema (json/parse-string (slurp (io/resource "ooapiv4.json"))
                               #(if (str/starts-with? % "/") % (keyword %))))

(def route-data
  {"/"                                                {:ooapi/cardinality :singleton
                                                       :ooapi/datatype :service}

   "/associations/{associationId}"                    {:ooapi/cardinality :one
                                                       :ooapi/id-path [:path-params :associationId]
                                                       :ooapi/datatype [:programOfferingAssociation :courseOfferingAssociation]
                                                       :ooapi/expand {"person" [:one [:programOfferingAssociation/person :courseOfferingAssociation/person]]
                                                                      "offering" [:one [:programOfferingAssociation/programOffering :courseOfferingAssociation/courseOffering] :offering]}}

   "/academic-sessions"                               {:ooapi/cardinality :many
                                                       :ooapi/datatype :academicSession ; TODO requires special filter to filter by parent and year
                                                       :ooapi/sort #{"startDate" "academicSessionId" "name"}}
   "/academic-sessions/{academicSessionId}"           {:ooapi/cardinality :one
                                                       :ooapi/datatype :academicSession
                                                       :ooapi/id-path [:path-params :academicSessionId]}
                                                       ;:ooapi/expand #{:parent :year}}
   "/academic-sessions/{academicSessionId}/offerings" {:ooapi/cardinality :many
                                                       :ooapi/datatype [:programOffering :courseOffering]
                                                       :ooapi/filters #{:type :mainLanguage :isLineItem}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/select {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                                      :path [:path-params :academicSessionId]}}

   "/courses"                                         {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/filters #{:level :modeOfDelivery}
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/sort #{"name" "ects" "courseId"}}
   "/courses/{courseId}"                              {:ooapi/cardinality :one
                                                       :ooapi/datatype :course
                                                       :ooapi/id-path [:path-params :courseId]
                                                       :ooapi/expand {"programs" [:many :course/program :programs]
                                                                      "coordinator" [:one :course/coordinator]
                                                                      "organization" [:one :course/organization]}}
   "/courses/{courseId}/offerings"                    {:ooapi/cardinality :many ; TODO: add filters for since and until
                                                       :ooapi/datatype :courseOffering
                                                       :ooapi/filters #{:mainLanguage :modeOfStudy :isLineItem}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/select {:refs #{:courseOffering/course}
                                                                      :path [:path-params :courseId]}}

   "/offerings/{offeringId}"                          {:ooapi/cardinality :one
                                                       :ooapi/id-path [:path-params :offeringId]
                                                       :ooapi/datatype [:programOfferingAssociation :courseOfferingAssociation]
                                                       :ooapi/expand {"program" [:one :programOffering/program]
                                                                      ;"programOffering" [:one] TODO fix this
                                                                      "course" [:one :courseOffering/course]
                                                                      ;"courseOffering" [:one] TODO fix this
                                                                      "organization" [:one [:programOffering/organization :courseOffering/organization]]
                                                                      "academicSession" [:one [:programOffering/academicSession :courseOffering/academicSession]]}}

   "/persons"                                         {:ooapi/cardinality :many
                                                       :ooapi/datatype :person
                                                       :ooapi/filters #{:affiliations}
                                                       :ooapi/sort #{"personId" "givenName" "surName" "displayName"}}
   "/persons/{personId}"                              {:ooapi/cardinality :one
                                                       :ooapi/datatype :person
                                                       :ooapi/id-path [:path-params :personId]}
   "/persons/{personId}/associations"                 {:ooapi/cardinality :many
                                                       :ooapi/datatype [:programOfferingAssociation :courseOfferingAssociation]
                                                       :ooapi/filters #{:type :role :state :result-state}
                                                       :ooapi/sort #{"associationId"}
                                                       :ooapi/select {:refs #{:courseOfferingAssociation/person
                                                                              :programOfferingAssociation/person}
                                                                      :path [:path-params :personId]}}

   "/programs"                                        {:ooapi/cardinality :many
                                                       :ooapi/datatype :program
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters #{:type :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy :crohoCreboCode}
                                                       :ooapi/sort #{"programId" "name" "ects"}}
   "/programs/{programId}"                            {:ooapi/cardinality :one
                                                       :ooapi/datatype :program
                                                       :ooapi/id-path [:path-params :programId]
                                                       :ooapi/expand {;"parent" [:one] TODO fix this
                                                                      ;"children" [:many] TODO fix this
                                                                      "organization" [:one :program/organization]}}
   "/programs/{programId}/courses"                    {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters  #{:level :modeOfDelivery}
                                                       :ooapi/sort #{"courseId" "name" "ects"}
                                                       :ooapi/select {:refs #{:course/program}
                                                                      :path [:path-params :programId]}}
   "/programs/{programId}/offerings"                  {:ooapi/cardinality :many ; TODO add date filters
                                                       :ooapi/datatype :programOffering
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters #{:mainLanguage :modeOfStudy :isLineItem}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/select {:refs #{:programOffering/program}
                                                                      :path [:path-params :programId]}}})

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