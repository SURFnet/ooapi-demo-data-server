(ns instant-ooapi.data
  (:require
    [cheshire.core :as json]
    [clojure.data.generators :as gen]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [nl.surf.demo-data.config :as config]
    [nl.surf.demo-data.world :as world]
    [remworks.markov-chain :as mc]))

(def text-spaces (->> "seeds/data.edn"
                      io/resource
                      slurp
                      read-string
                      (map #(dissoc % :id :field-of-study))
                      (reduce (fn [m x]
                                (merge-with (fn [a b]
                                              (let [b (str/replace (str b) #"<[^>]+>" "")]
                                                (if (str/ends-with? a ".")
                                                  (str a " " b)
                                                  (str a ". " b))))
                                            m x))
                              {})
                      (map (juxt (comp name key)
                                 (comp mc/analyse-text val)))
                      (into {})))

(defmethod config/generator "lorem-surf" [_]
  (fn surf-lorem [_ & [scope lines]]
    (let [space (get text-spaces scope (get text-spaces "description"))]
      (->> #(mc/generate-text space)
           (repeatedly (or lines 3))
           (str/join " ")))))

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

(defmethod config/generator "arrayize" [_]
  (fn arrayize [_ x]
    (when x [x])))

(defn modify-org-hack
  "Very ugly hack to make sure there is one root organization
  which has the env variables ORGNAME and SHORTORGNAME as its
  name and shortName."
  [data]
  (let [org-name (or (System/getenv "ORGNAME") "RootOrganisatie")
        short-org-name (or (System/getenv "SHORTORGNAME") "RO")
        is-root? (comp #{"root"} :organization/type)
        root-orgs (->> data :organization (filter is-root?))
        root (-> (first root-orgs)
                 (assoc :organization/name org-name)
                 (assoc :organization/shortName short-org-name))
        rest-orgs (->> (rest root-orgs)
                       (map #(assoc % :organization/type "department")))
        orgs (->> data
                  :organization
                  (remove is-root?)
                  (concat [root])
                  (concat rest-orgs)
                  vec)]
    (assoc data :organization orgs)))

;; use ooapi version specific resource files
(def ooapi-version (or (System/getenv "OOAPIVERSION") "v5"))
(def schema-file (str "schema" ooapi-version ".json"))
(def ooapi-file (str "ooapi" ooapi-version ".json"))

(defn generate-data
  []
  (-> schema-file
      (io/resource)
      (slurp)
      (config/load-json)
      (world/gen (-> "pop.edn"
                     (io/resource)
                     (slurp)
                     (edn/read-string)))))

(def data (modify-org-hack (generate-data)))

(def schema (json/parse-string (slurp (io/resource ooapi-file))
                               #(if (str/starts-with? % "/") % (keyword %))))

(def route-data-v4
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
                                                                      :path [:path-params :courseId]}
                                                       :ooapi/expands #{:courseOffering/academicSession}}

   "/offerings/{offeringId}"                          {:ooapi/cardinality :one
                                                       :ooapi/id-path [:path-params :offeringId]
                                                       :ooapi/datatype [:programOffering :courseOffering]
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
                                                                      :path [:path-params :programId]}
                                                       :ooapi/expands #{:programOffering/academicSession}}

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
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/expands #{:programOffering/academicSession :courseOffering/academicSession}}})
                                                       ;:ooapi/select {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                       ;               :path [:path-params :organizationId]}}})

(def route-data-v5
  {"/"                                                {:ooapi/cardinality :singleton
                                                       :ooapi/datatype :service}
   "/organizations"                                   {:ooapi/cardinality :many
                                                       :ooapi/datatype :organization
                                                       :ooapi/filters #{:organizationType}
                                                       :ooapi/sort #{"name""organizationId"}}
   "/organizations/{organizationId}"                  {:ooapi/cardinality :one
                                                       :ooapi/datatype :organization
                                                       :ooapi/id-path [:path-params :organizationId]}
   "/organizations/{organizationId}/programs"         {:ooapi/cardinality :many
                                                       :ooapi/datatype :program
                                                       :ooapi/filters #{:teachingLanguage :programType :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy}
                                                       :ooapi/select {:refs #{:program/organization}
                                                                      :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/courses"          {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/filters #{:teachingLanguage :level :modeOfDelivery}
                                                       :ooapi/select {:refs #{:course/organization}
                                                                      :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/offerings"        {:ooapi/cardinality :many
                                                       :ooapi/datatype [:programOffering :courseOffering]
                                                       :ooapi/filters #{:teachingLanguage :offeringType :resultExpected}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/expands #{:programOffering/academicSession :courseOffering/academicSession}}
                                                       ;:ooapi/select {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                       ;               :path [:path-params :organizationId]}}})
   "/programs"                                        {:ooapi/cardinality :many
                                                       :ooapi/datatype :program
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters #{:teachingLanguage :programType :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy}
                                                       :ooapi/sort #{"programId" "name"}}
   "/programs/{programId}"                            {:ooapi/cardinality :one
                                                       :ooapi/datatype :program
                                                       :ooapi/id-path [:path-params :programId]
                                                       :ooapi/expand {;"parent" [:one] TODO fix this
                                                                      ;"children" [:many] TODO fix this
                                                                      "coordinator" [:one :program/coordinator]
                                                                      "organization" [:one :program/organization]}}
   "/programs/{programId}/courses"                    {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters #{:teachingLanguage :level :modeOfDelivery}
                                                       :ooapi/sort #{"courseId" "name"}
                                                       :ooapi/select {:refs #{:course/program}
                                                                      :path [:path-params :programId]}}
   "/programs/{programId}/offerings"                  {:ooapi/cardinality :many ; TODO add date filters
                                                       :ooapi/datatype :programOffering
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/filters #{:teachingLanguage :offeringType :resultExpected}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/select {:refs #{:programOffering/program}
                                                                      :path [:path-params :programId]}
                                                       :ooapi/expands #{:programOffering/academicSession}}
   "/courses"                                         {:ooapi/cardinality :many
                                                       :ooapi/datatype :course
                                                       :ooapi/filters #{:teachingLanguage :level :modeOfDelivery}
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/sort #{"name" "courseId"}}
   "/courses/{courseId}"                              {:ooapi/cardinality :one
                                                       :ooapi/datatype :course
                                                       :ooapi/id-path [:path-params :courseId]
                                                       :ooapi/expand {"programs" [:many :course/program :programs]
                                                                      "coordinator" [:one :course/coordinator]
                                                                      "organization" [:one :course/organization]}}
   "/courses/{courseId}/offerings"                    {:ooapi/cardinality :many ; TODO: add filters for since and until
                                                       :ooapi/datatype :courseOffering
                                                       :ooapi/filters #{:teachingLanguage :offeringType :resultExpected}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/select {:refs #{:courseOffering/course}
                                                                      :path [:path-params :courseId]}
                                                       :ooapi/expands #{:courseOffering/academicSession}}
   "/offerings/{offeringId}"                          {:ooapi/cardinality :one
                                                       :ooapi/id-path [:path-params :offeringId]
                                                       :ooapi/datatype [:programOffering :courseOffering]
                                                       :ooapi/expand {"program" [:one :programOffering/program]
                                                                      ;"programOffering" [:one] TODO fix this
                                                                      "course" [:one :courseOffering/course]
                                                                      ;"courseOffering" [:one] TODO fix this
                                                                      "organization" [:one [:programOffering/organization :courseOffering/organization]]
                                                                      "academicSession" [:one [:programOffering/academicSession :courseOffering/academicSession]]}}
   "/academic-sessions"                               {:ooapi/cardinality :many
                                                       :ooapi/datatype :academicSession ; TODO requires special filter to filter by parent and year
                                                       :ooapi/filters #{:academicSessionType}
                                                       :ooapi/sort #{"startDate" "academicSessionId" "name"}}
   "/academic-sessions/{academicSessionId}"           {:ooapi/cardinality :one
                                                       :ooapi/datatype :academicSession
                                                       :ooapi/id-path [:path-params :academicSessionId]}
                                                       ;:ooapi/expand #{:parent :year}}
   "/academic-sessions/{academicSessionId}/offerings" {:ooapi/cardinality :many
                                                       :ooapi/datatype [:programOffering :courseOffering]
                                                       :ooapi/filters #{:teachingLanguage :offeringType :resultExpected}
                                                       :ooapi/sort #{"startDate" "offeringId" "name" "endDate"}
                                                       :ooapi/q-fields #{:name :abbreviation :description}
                                                       :ooapi/select {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                                      :path [:path-params :academicSessionId]}
                                                       :ooapi/expands #{:courseOffering/academicSession :programOffering/academicSession}}
   "/persons"                                         {:ooapi/cardinality :many
                                                       :ooapi/datatype :person
                                                       :ooapi/filters #{:affiliations}
                                                       :ooapi/sort #{"personId" "givenName" "surName" "displayName"}}
   "/persons/{personId}"                              {:ooapi/cardinality :one
                                                       :ooapi/datatype :person
                                                       :ooapi/id-path [:path-params :personId]}
                                                       })

;; use ooapi version specific data
(def route-data (case ooapi-version
  "v4" route-data-v4
  "v5" route-data-v5))

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