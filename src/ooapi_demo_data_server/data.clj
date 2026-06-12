(ns ooapi-demo-data-server.data
  (:require
   [cheshire.core :as json]
   [clojure.data.generators :as gen]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [nl.surf.demo-data.config :as config]
   [nl.surf.demo-data.world :as world]
   [remworks.markov-chain :as mc]
   [ooapi-demo-data-server.common :as common])
  (:import java.util.Calendar))

;; use ooapi version specific resource files
(def ^:dynamic ooapi-version (or (System/getenv "OOAPI_VERSION") "v6"))

(def schema-file (str ooapi-version "/schema.json"))

(def ooapi-file (str ooapi-version "/ooapi.json"))

(def pop-file (str ooapi-version "/pop.edn"))

(def text-spaces (->> "seeds/data.edn"
                      io/resource
                      slurp
                      edn/read-string
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

; This function has a 50% chance of returing zero items
(defmethod config/generator "zero-or-more-of" [_]
  (fn zero-or-more-of [_ xs]
    (when (seq xs)
      (let [pick? (gen/boolean)
            n (gen/uniform 1 (inc (count xs)))]
        (if pick?
          (take n (gen/shuffle xs))
          '())))))

(defmethod config/generator "array" [_]
  (fn array [_ & xs]
    (when (seq xs)
      xs)))

(defmethod config/generator "cons" [_]
  (fn _cons [_ x xs]
    (cons x xs)))

(defmethod config/generator "arrayize" [_]
  (fn arrayize [_ x]
    (when x [x])))

(defn to-language-typed-string
  [s]
  (let [en {:language "en-GB"}
        nl {:language "nl-NL"}]
    [(assoc en :value (str "EN TRANSLATION: " s))
     (assoc nl :value (str "NL VERTALING: " s))]))

(defmethod config/generator "language-typed-string" [_]
  (fn language-typed-string [_ s]
    (to-language-typed-string s)))

(defmethod config/generator "language-typed-strings" [_]
  (fn language-typed-strings [_ strings]
    (map to-language-typed-string strings)))

(defmethod config/generator "mapping" [_]
  (fn mapping [_ m k]
    (get m k #_(throw (ex-info "Incorrect mapping" {:m m :k k})))))

#_(defmethod config/generator "optional" [_]
    (fn optional [_ value chance]
      (gen/weighted {(constantly nil) 10
                     (constantly value) chance})))

(defmethod config/generator "object-with-optionals" [_]
  (fn object-with-optionals [_ choices & keyvals]
    (assert (even? (count keyvals)))
    (let [n (/ (count keyvals) 2)
          ks (take n keyvals)
          vs (->> keyvals
                  (drop n)
                  (take n))
          result (zipmap ks vs)]
      (->> (for [[k v] result]
             (if-let [choice (get choices k)]
               (when (> choice (gen/float)) [k v])
               [k v]))
           (remove nil?)
           (into {})))))

(defmethod config/generator "later-date" [_]
  (fn later-date
    ([_ min-days max-days date]
     (let [days-to-add (gen/uniform min-days max-days)
           new-date (doto (.clone date)
                      (.add Calendar/DAY_OF_MONTH days-to-add))]
       new-date))
    ([world date]
     (later-date world 1 365 date))))

(defmethod config/generator "ref-with-constraints" [_]
  (fn ref-with-constraints
    [context dep constraint & _deps]
    (let [world (:world context)
          dep (keyword dep)
          entity-kw (-> dep namespace keyword)
          candidates (get world entity-kw)
          valid-candidates (filter (comp (set (second constraint)) (-> constraint first keyword)) candidates)]
      (when-let [entity (first (gen/shuffle valid-candidates))]
        [dep (get entity dep)]))))

(defn modify-org-hack
  "Very ugly hack to make sure there is one root organization
  which has the env variables ORGNAME and SHORTORGNAME as its
  name and shortName."
  [data]
  (let [org-name (or (System/getenv "ORGNAME") "RootOrganisatie")
        short-org-name (or (System/getenv "SHORTORGNAME") "RO")
        is-root? (comp #{"root"} (case ooapi-version
                                   "v4" :organization/type
                                   "v5" :organization/organizationType))
        root-orgs (->> data :organization (filter is-root?))
        root (cond-> (first root-orgs)

               (= ooapi-version "v4")
               (assoc :organization/name org-name)

               (= ooapi-version "v5")
               (assoc :organization/name (to-language-typed-string org-name))

               true
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

(defn modify-org-hack-v6
  "Very ugly hack to make sure there is one root organization
  which has the env variables ORGNAME and SHORTORGNAME as its
  name and shortName."
  [data]
  (let [org-name (or (System/getenv "ORGNAME") "RootOrganisatie")
        short-org-name (or (System/getenv "SHORTORGNAME") "RO")
        is-root? (comp #{"root"} :organisation/organisationType)
        root-orgs (->> data :organisation (filter is-root?))
        root (cond-> (first root-orgs)

               true
               (assoc :organisation/name (to-language-typed-string org-name))

               true
               (assoc :organisation/name short-org-name))
        rest-orgs (->> (rest root-orgs)
                       (map #(assoc % :organisation/name "department")))
        orgs (->> data
                  :organisation
                  (remove is-root?)
                  (concat [root])
                  (concat rest-orgs)
                  vec)]
    (assoc data :organisation orgs)))

(defn modify-org-hack-switcher
  [data]
  (case ooapi-version
    "v4" (modify-org-hack data)
    "v5" (modify-org-hack data)
    "v6" (modify-org-hack-v6 data)))

(defn add-children-attr
  [data entity-name self-attr-name parent-attr-name children-attr-name]
  (let [make-ref (fn [id] [self-attr-name id])
        entities (get data entity-name)
        mapping (reduce
                 (fn [mapping entity]
                   (let [self-id (get entity self-attr-name)
                         [_ parent-id] (get entity parent-attr-name)]
                     (if parent-id
                       (update mapping parent-id (fnil conj #{}) self-id)
                       mapping)))
                 {}
                 entities)
        new-entities (for [entity entities]
                       (let [self-id (get entity self-attr-name)]
                         (if-let [children (get mapping self-id)]
                           (assoc entity children-attr-name (map make-ref children))
                           entity)))]
    (assoc data entity-name (vec new-entities))))

(defn clean-empty-parents
  [data entity-name parent-attr-name]
  (let [entities (get data entity-name)
        clean-fn (fn [entity] (let [[_ parent-uuid] (get entity parent-attr-name)]
                                (if parent-uuid
                                  entity
                                  (dissoc entity parent-attr-name))))]
    (assoc data entity-name (map clean-fn entities))))

(defn generate-data
  []
  (let [data (-> schema-file
                 (io/resource)
                 (slurp)
                 (config/load-json)
                 (world/gen (-> pop-file
                                (io/resource)
                                (slurp)
                                (edn/read-string))))]
    (cond-> data

      true
      (modify-org-hack-switcher)

      (= ooapi-version "v5")
      (clean-empty-parents :educationSpecification :educationSpecification/parent)

      (= ooapi-version "v5")
      (add-children-attr :educationSpecification :educationSpecification/educationSpecificationId :educationSpecification/parent :educationSpecification/children)

      (= ooapi-version "v5")
      (clean-empty-parents :academicSession :academicSession/parent)

      (= ooapi-version "v6")
      (clean-empty-parents :academicSession :academicSession/parentId)

      (= ooapi-version "v5")
      (add-children-attr :academicSession :academicSession/academicSessionId :academicSession/parent :academicSession/children)

      (= ooapi-version "v6")
      (add-children-attr :academicSession :academicSession/academicSessionId :academicSession/parentId :academicSession/childrenIds)

      (= ooapi-version "v5")
      (clean-empty-parents :program :program/parent)

      (= ooapi-version "v6")
      (clean-empty-parents :programme :programme/parentId)

      (= ooapi-version "v5")
      (add-children-attr :program :program/programId :program/parent :program/childrenIds)

      (= ooapi-version "v6")
      (add-children-attr :programme :programme/programmeId :programme/parentId :programme/childrenIds)
      )))

(def data (generate-data))

(def schema (json/parse-string (slurp (io/resource ooapi-file))
                               #(if (str/starts-with? % "/") % (keyword %))))

(def route-data-v4
  {"/"                                                {:ooapi/cardinality :singleton
                                                       :ooapi/datatype :service}

   "/associations/{associationId}"                    {:ooapi/cardinality :one
                                                       :ooapi/id-path [:path-params :associationId]
                                                       :ooapi/datatype [:programOfferingAssociation :courseOfferingAssociation]
                                                       :ooapi/expands {:programOfferingAssociation/person :courseOfferingAssociation/person
                                                                       :programOfferingAssociation/programOffering :courseOfferingAssociation/courseOffering}}

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
                                                       :ooapi/expands #{:course/program :course/coordinator :course/organization}}
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
                                                       :ooapi/expands #{:programOffering/program :programOffering/organization :programOffering/academicSession
                                                                        :courseOffering/course :courseOffering/organization :courseOffering/academicSession}}
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
                                                       :ooapi/expands #{:program/organization}}
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

(def route-data-v5
  {"/"                                                                             {:ooapi/cardinality :singleton
                                                                                    :ooapi/datatype    :service}
   "/organizations"                                                                {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :organization
                                                                                    :ooapi/filters     #{:organizationType}
                                                                                    :ooapi/sort        #{"name" "organizationId"}}
   "/organizations/{organizationId}"                                               {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :organization
                                                                                    :ooapi/id-path     [:path-params :organizationId]
                                                                                    :ooapi/expands     #{:organization/parent :organization/children}}
   "/organizations/{organizationId}/programs"                                      {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :program
                                                                                    :ooapi/filters     #{:teachingLanguage :programType :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy}
                                                                                    :ooapi/select      {:refs #{:program/organization}
                                                                                                        :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/courses"                                       {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/filters     #{:teachingLanguage :level :modeOfDelivery}
                                                                                    :ooapi/select      {:refs #{:course/organization}
                                                                                                        :path [:path-params :organizationId]}}
   "/organizations/{organizationId}/offerings"                                     {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    [:programOffering :courseOffering]
                                                                                    :ooapi/filters     #{:teachingLanguage :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDate" "offeringId" "name" "endDate"}
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}}
   "/education-specifications"                                                     {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :educationSpecification
                                                                                    :ooapi/filters     #{:educationSpecificationType}
                                                                                    :ooapi/sort        #{"educationSpecificationType" "name" "primaryCode"}}
   "/education-specifications/{educationSpecificationId}"                          {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :educationSpecification
                                                                                    :ooapi/id-path     [:path-params :educationSpecificationId]
                                                                                    :ooapi/expands     #{:educationSpecification/parent :educationSpecification/children :educationSpecification/organization}}
   "/education-specifications/{educationSpecificationId}/education-specifications" {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :educationSpecification
                                                                                    :ooapi/select      {:refs #{:educationSpecification/parent}
                                                                                                        :path [:path-params :educationSpecificationId]}}
   "/education-specifications/{educationSpecificationId}/programs"                 {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :program
                                                                                    :ooapi/select      {:refs #{:program/educationSpecification}
                                                                                                        :path [:path-params :educationSpecificationId]}}
   "/education-specifications/{educationSpecificationId}/courses"                  {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/select      {:refs #{:course/educationSpecification}
                                                                                                        :path [:path-params :educationSpecificationId]}}
   "/programs"                                                                     {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :program
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage :programType :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy}
                                                                                    :ooapi/sort        #{"programId" "name"}}
   "/programs/{programId}"                                                         {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :program
                                                                                    :ooapi/id-path     [:path-params :programId]
                                                                                    :ooapi/expands     #{:program/parent :program/children :program/coordinators :program/organization :program/educationSpecification}}
   "/programs/{programId}/programs"                                                {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :program
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage  :programType :qualificationAwarded :levelOfQualification :sector :fieldsOfStudy}
                                                                                    :ooapi/sort        #{"programId" "name"}
                                                                                    :ooapi/select      {:refs #{:program/parent}
                                                                                                        :path [:path-params :programId]}}
   "/programs/{programId}/courses"                                                 {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage :level :modeOfDelivery}
                                                                                    :ooapi/sort        #{"courseId" "name"}
                                                                                    :ooapi/select      {:refs #{:course/program}
                                                                                                        :path [:path-params :programId]}}
   "/programs/{programId}/offerings"                                               {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :programOffering
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDate" "offeringId" "name" "endDate"}
                                                                                    :ooapi/select      {:refs #{:programOffering/program}
                                                                                                        :path [:path-params :programId]}
                                                                                    :ooapi/expands     #{:programOffering/academicSession}}
   "/courses"                                                                      {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/filters     #{:teachingLanguage :level :modeOfDelivery}
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/sort        #{"name" "courseId"}}
   "/courses/{courseId}"                                                           {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/id-path     [:path-params :courseId]
                                                                                    :ooapi/expands     #{:course/coordinators :course/programs :course/organization :course/educationSpecification}}
   "/courses/{courseId}/offerings"                                                 {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :courseOffering
                                                                                    :ooapi/filters     #{:teachingLanguage :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDate" "offeringId" "name" "endDate"}
                                                                                    :ooapi/select      {:refs #{:courseOffering/course}
                                                                                                        :path [:path-params :courseId]}}
   "/offerings/{offeringId}"                                                       {:ooapi/cardinality :one
                                                                                    :ooapi/id-path     [:path-params :offeringId]
                                                                                    :ooapi/datatype    [:programOffering :courseOffering]
                                                                                    :ooapi/expands     #{:programOffering/program :programOffering/organization :programOffering/academicSession
                                                                                                         :courseOffering/course :courseOffering/organization :courseOffering/academicSession}}
   "/academic-sessions"                                                            {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :academicSession
                                                                                    :ooapi/filters     #{:academicSessionType}
                                                                                    :ooapi/sort        #{"startDate" "academicSessionId" "name"}}
   "/academic-sessions/{academicSessionId}"                                        {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :academicSession
                                                                                    :ooapi/id-path     [:path-params :academicSessionId]
                                                                                    :ooapi/expands     #{:academicSession/parent :academicSession/children}}
   "/academic-sessions/{academicSessionId}/offerings"                              {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    [:programOffering :courseOffering]
                                                                                    :ooapi/filters     #{:teachingLanguage :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDate" "offeringId" "name" "endDate"}
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/select      {:refs #{:programOffering/academicSession :courseOffering/academicSession}
                                                                                                        :path [:path-params :academicSessionId]}}
   "/persons"                                                                      {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :person
                                                                                    :ooapi/filters     #{:affiliations}
                                                                                    :ooapi/sort        #{"personId" "givenName" "surName" "displayName"}}
   "/persons/{personId}"                                                           {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :person
                                                                                    :ooapi/id-path     [:path-params :personId]}})

(def route-data-v6
  {"/"                                                                             {:ooapi/cardinality :singleton
                                                                                    :ooapi/datatype    :service}
   "/organisations"                                                                {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :organisation
                                                                                    :ooapi/filters     #{:organisationType}
                                                                                    :ooapi/sort        #{"name" "organisationId"}}
   "/organisations/{organisationId}"                                                {:ooapi/cardinality :one
                                                                                     :ooapi/datatype    :organisation
                                                                                     :ooapi/id-path     [:path-params :organisationId]
                                                                                     :ooapi/expands     #{:organisation/parent :organisation/children}}
   "/organisations/{organisationId}/course-offerings"                               {:ooapi/cardinality :many
                                                                                     :ooapi/datatype    :courseOffering
                                                                                     :ooapi/id-path     [:path-params :organisationId]
                                                                                     :ooapi/expands     #{:organisation/parent :organisation/children}
                                                                                     :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                     :ooapi/sort        #{"startDateTime" "offeringId" "name" "endDateTime"}
                                                                                     :ooapi/select      {:refs #{:courseOffering/organisationId}
                                                                                                         :path [:path-params :organisationId]}}
   "/organisations/{organisationId}/programme-offerings"                            {:ooapi/cardinality :many
                                                                                     :ooapi/datatype    :programmeOffering
                                                                                     :ooapi/id-path     [:path-params :organisationId]
                                                                                     :ooapi/expands     #{:organisation/parent :organisation/children}
                                                                                     :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                     :ooapi/sort        #{"startDateTime" "offeringId" "name" "endDateTime"}
                                                                                     :ooapi/select      {:refs #{:programmeOffering/organisationId}
                                                                                                         :path [:path-params :organisationId]}}
   "/organisations/{organisationId}/programmes"                                     {:ooapi/cardinality :many
                                                                                     :ooapi/datatype    :programme
                                                                                     :ooapi/id-path     [:path-params :organisationId]
                                                                                     :ooapi/expands     #{:organisation/parent :organisation/children}
                                                                                     :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                     :ooapi/filters     #{:teachingLanguage :programmeType :qualificationAwarded :levelOfQualification :fieldsOfStudy}
                                                                                     :ooapi/sort        #{"programmeId" "name"}
                                                                                     :ooapi/select      {:refs #{:programme/organisationId}
                                                                                                         :path [:path-params :organisationId]}}

   "/academic-sessions"                                                            {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :academicSession
                                                                                    :ooapi/filters     #{:academicSessionType}
                                                                                    :ooapi/sort        #{"startDateTime" "academicSessionId" "name"}}
   "/academic-sessions/{academicSessionId}"                                        {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :academicSession
                                                                                    :ooapi/id-path     [:path-params :academicSessionId]
                                                                                    :ooapi/expands     #{:academicSession/parent :academicSession/children}}

   "/academic-sessions/{academicSessionId}/course-offerings"                       {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :courseOffering
                                                                                    :ooapi/id-path     [:path-params :academicSessionId]
                                                                                    :ooapi/expands     #{:academicSession/parent :academicSession/children}
                                                                                    :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDateTime" "offeringId" "name" "endDateTime"}
                                                                                    :ooapi/select      {:refs #{:courseOffering/academicSessionId}
                                                                                                        :path [:path-params :academicSessionId]}}
   "/academic-sessions/{academicSessionId}/programme-offerings"                    {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :programmeOffering
                                                                                    :ooapi/id-path     [:path-params :academicSessionId]
                                                                                    :ooapi/expands     #{:academicSession/parent :academicSession/children}
                                                                                    :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDateTime" "offeringId" "name" "endDateTime"}
                                                                                    :ooapi/select      {:refs #{:programmeOffering/academicSessionId}
                                                                                                        :path [:path-params :academicSessionId]}}
   "/courses"                                                                      {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/filters     #{:teachingLanguage :level :modeOfDelivery}
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/sort        #{"name" "courseId"}}
   "/courses/{courseId}"                                                           {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/id-path     [:path-params :courseId]
                                                                                    :ooapi/expands     #{:course/coordinators :course/programmes :course/organisation :course/learningOutcomes}}
   "/courses/{courseId}/course-offerings"                                          {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :courseOffering
                                                                                    :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDateTime" "offeringId" "name" "endDateTime"}
                                                                                    :ooapi/select      {:refs #{:courseOffering/courseId}
                                                                                                        :path [:path-params :courseId]}}
   "/programmes"                                                                   {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :programme
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage :programmeType :qualificationAwarded :levelOfQualification :fieldsOfStudy}
                                                                                    :ooapi/sort        #{"programmeId" "name"}}
   "/programmes/{programmeId}"                                                     {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :programme
                                                                                    :ooapi/id-path     [:path-params :programmeId]
                                                                                    :ooapi/expands     #{:programme/parent :programme/children :programme/coordinators :programme/organisation :programme/learningOutcomes}}
   "/programmes/{programmeId}/programmes"                                          {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :programme
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguage  :programmeType :qualificationAwarded :levelOfQualification :fieldsOfStudy}
                                                                                    :ooapi/sort        #{"programmeId" "name"}
                                                                                    :ooapi/select      {:refs #{:programme/parentId}
                                                                                                        :path [:path-params :programmeId]}}
   "/programmes/{programmeId}/courses"                                             {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :course
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguages :level :modeOfDelivery}
                                                                                    :ooapi/sort        #{"courseId" "name"}
                                                                                    :ooapi/select      {:refs #{:course/programmeIds}
                                                                                                        :path [:path-params :programmeId]}}
   "/programmes/{programmeId}/programme-offerings"                                 {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :programmeOffering
                                                                                    :ooapi/q-fields    #{:name :abbreviation :description}
                                                                                    :ooapi/filters     #{:teachingLanguages :offeringType :resultExpected}
                                                                                    :ooapi/sort        #{"startDate" "offeringId" "name" "endDate"}
                                                                                    :ooapi/select      {:refs #{:programmeOffering/programmeId}
                                                                                                        :path [:path-params :programmeId]}}
   "/persons"                                                                      {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :person
                                                                                    :ooapi/filters     #{:affiliations}
                                                                                    :ooapi/sort        #{"personId" "givenName" "surname" "displayName"}}
   "/persons/{personId}"                                                           {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :person
                                                                                    :ooapi/id-path     [:path-params :personId]}
   "/learning-outcomes"                                                            {:ooapi/cardinality :many
                                                                                    :ooapi/datatype    :learningOutcome}
   "/learning-outcomes/{learningOutcomeId}"                                        {:ooapi/cardinality :one
                                                                                    :ooapi/datatype    :learningOutcome
                                                                                    :ooapi/id-path     [:path-params :learningOutcomeId]}
   })

;; use ooapi version specific data
(def route-data
  (case ooapi-version
    "v4" route-data-v4
    "v5" route-data-v5
    "v6" route-data-v6))

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
       :parameters))

(comment
  (def test-data (generate-data))

  (keys test-data)

  (->> test-data
       :courseOffering
       rand-nth
       :courseOffering/consumers)


  (def test-schema
    (-> schema-file
        (io/resource)
        (slurp)
        (json/parse-string true)))

  (require '[ooapi-demo-data-server.common :as common])

  (->> test-schema
       :types
       (common/index-by (comp keyword :name))
       :courseOffering
       :attributes))
