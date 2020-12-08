(ns instant-ooapi.data
  (:require
    [cheshire.core :as json]
    [clojure.data.generators :as gen]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
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

(defmethod config/generator "at-least-one-of" [_]
  (fn at-least-one-of [_ xs]
    (when (seq xs)
      (let [n (gen/uniform 1 (count xs))]
        (take n (gen/shuffle xs))))))

(defn generate-data
  []
  (-> "schema.json"
      (io/resource)
      (slurp)
      (config/load-json)
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
                                                                      :path [:path-params :academicSessionId]}
                                                       :ooapi/expands #{:courseOffering/academicSession :programOffering/academicSession}}

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
                                                                      :path [:path-params :programId]}}

   "/organizations"                                   {:ooapi/cardinality :many
                                                       :ooapi/datatype :organization
                                                       :ooapi/filters #{:type}}
   "/organizations/{organizationId}"                  {:ooapi/cardinality :one
                                                       :ooapi/datatype :organization
                                                       :ooapi/id-path [:path-params :organizationId]}
                                                       ;:ooapi/expand {}}
   "/organizations/{organizationId}/programs"         {:ooapi/cardinality :many
                                                       :ooapi/datatype :program
                                                       :ooapi/filters #{:type :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy :crohoCreboCode}
                                                       :ooapi/select {:refs #{:program/organization}
                                                                      :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/courses"          {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/filters #{:level :modeOfDelivery}
                                                       :ooapi/select {:refs #{:course/organization}
                                                                      :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/offerings"        {:ooapi/cardinality :many
                                                       :ooapi/datatype [:programOffering :courseOffering]
                                                       :ooapi/filters #{:type :mainLanguage :isLineItem}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/q-fields #{:name :abbreviation :description}}})
                                                       ;:ooapi/select {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                       ;               :path [:path-params :organizationId]}}})

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