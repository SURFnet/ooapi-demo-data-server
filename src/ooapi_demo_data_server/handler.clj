(ns ooapi-demo-data-server.handler
  (:require
   [cheshire.core :as json]
   [cheshire.generate :as jsong]
   [clojure.instant :refer [read-instant-date]]
   [clojure.string :as str]
   [clojure.set :refer [rename-keys intersection difference]]
   [integrant.core :as ig]
   [clojure.tools.logging :as log]
   [ooapi-demo-data-server.data :as data]
   [ooapi-demo-data-server.common :as common]
   [reitit.ring :as ring]
   [ring.middleware.params :as params]))

(defmulti coerce-parameter (fn [schema _] (:type schema)))

(defmethod coerce-parameter :default
  [schema value]
  (log/warn :coercion/unknown-type {:type (:type schema)
                                    :schema schema
                                    :value value})
  value)

(defmethod coerce-parameter "integer"
  [{:keys [enum default]} value]
  (let [i (cond
            (and (nil? value) default) default
            (nil? value)               nil
            :else                      (Integer/parseInt value))]
    (if enum
      ((set enum) i) ; TODO add error here if not part of enum
      i)))

(defmulti parse-string (fn [format _] format))

(defmethod parse-string "uuid"
  [_ s]
  (when s
    (java.util.UUID/fromString s)))

(defmethod parse-string "date"
  [_ s]
  (when s
    (read-instant-date s)))

(defmethod coerce-parameter "string"
  [{:keys [enum format default]} value]
  (let [s (cond
            (and (nil? value) (not format) default) default
            (nil? value)                            nil
            :else                                   value)]
    (cond
      format (parse-string format s)
      enum   ((set enum) s)
      :else  s)))

(defmethod coerce-parameter "array"
  [{:keys [enum default]} value]
  (let [a (cond
            (and (nil? value) default) default
            (nil? value)               nil
            :else                      value)]
    (cond-> a
      (string? a) (str/split #",")
      enum        (->> a
                       (filter (set enum))
                       (into [])))))

; HERMAN: in v5 parameter & parameter.schema kan een $ref zijn
; use the script 'fixRefs.js' to get rid of those refs in the ooapiv5.json file
; could probably be fixed here as well
(defn req->param-schemas
  [req source]
  (->> req
       ring/get-match
       :data
       :parameters
       (filter (comp #{source} :in))
       (map (juxt :name :schema))
       (into {})))

(defn coerce-query-parameters
  [{:keys [query-params] :as req}]
  (let [schemas (req->param-schemas req "query")
        params (for [[param schema] schemas]
                 [(keyword param)
                  (coerce-parameter schema (get query-params param))])]
    (assoc req :query-params (->> params
                                  (filter second) ; remove empty values
                                  (into {})))))

(defn coerce-path-parameters
  [{:keys [path-params] :as req}]
  (let [schemas (req->param-schemas req "path")
        params (for [[param schema] schemas]
                 [(keyword param)
                  (coerce-parameter schema (get path-params (keyword param)))])]
    (assoc req :path-params (->> params
                                 (filter second) ; remove empty values
                                 (into {})))))

(defn req->cardinality
  [req]
  (-> req ring/get-match :data :ooapi/cardinality))

(defn req->datatype
  [req]
  (-> req ring/get-match :data :ooapi/datatype))

(defn req->id-path
  [req]
  (-> req ring/get-match :data :ooapi/id-path))

(defn req->id
  [req]
  (let [id-path (req->id-path req)]
    (get-in req id-path)))

(defn combine-kw
  [k1 k2]
  (keyword (name k1) (name k2)))

(defn req->select-refs
  [req]
  (-> req ring/get-match :data :ooapi/select :refs))

(defn req->select-path
  [req]
  (-> req ring/get-match :data :ooapi/select :path))

(defn apply-select
  [req items]
  (let [select-path (req->select-path req)
        select-id (get-in req select-path)
        select-refs (req->select-refs req)
        selected? (fn [item]
                    (contains?
                     (->> (select-keys item select-refs)
                          (vals)
                          (map second)
                          (remove nil?)
                          (into #{}))
                     select-id))]
    (if select-path
      (filter selected? items)
      items)))

(defn req->filters
  [{:keys [query-params] :as req}]
  (let [datatype (req->datatype req)
        filters (-> req ring/get-match :data :ooapi/filters)]
    (->> filters
         (select-keys query-params)
         (map (fn [[k v]] [(combine-kw datatype k) v])))))
         ; TODO fix filters now that datatype can be a vector

(defn apply-filters
  [req items]
  (let [filters (req->filters req)
        apply-filter (fn [items [field value]]
                       (filter (comp #{value} field) items))]
    (reduce apply-filter items filters)))

(defn apply-pagination
  [size number items]
  (->> items
       (drop (* size (dec number))) ; we decrease by one because first page number is 1
       (take size)))

(defn get-items
  [req]
  (let [datatype (req->datatype req)]
    (if (vector? datatype)
      (reduce
       (fn [items dt] (concat items (get data/data dt)))
       []
       datatype)
      (get data/data datatype))))

(jsong/add-encoder java.util.GregorianCalendar
                   (fn [c jsonGenerator]
                     (.writeString jsonGenerator (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (.getTime c)))))

(defn single-ref?
  [x]
  (and (vector? x)
       (qualified-keyword? (first x))
       (uuid? (second x))))

(defn many-ref?
  [x]
  (boolean
   (when x
     (and (coll? x)
          (every? single-ref? x)))))

(defn ref?
  [x]
  (or (single-ref? x)
      (many-ref? x)))

(defn resolve-single-ref
  [[attr id]]
  (let [datatype (keyword (namespace attr))
        items (get data/data datatype)
        indexed-items (common/index-by attr items)]
    (get indexed-items id)))

(defn resolve-many-refs
  [refs]
  (map resolve-single-ref refs))

(defn resolve-ref
  [ref]
  (if (many-ref? ref)
    (resolve-many-refs ref)
    (resolve-single-ref ref)))

;; WHY ARE THE FOLLOWING TO FUNCTIONS DIFFERENT?
(defn req->expands
  [req]
  (-> req ring/get-match :data :ooapi/expands))

(defn calc-total-pages
  [page-size n-items]
  (let [q (quot n-items page-size)
        r (rem n-items page-size)]
    (if (zero? r)
      q
      (inc q))))

(defn clean-attr
  [item attr] 
  (case data/ooapi-version
    "v4"
    (dissoc item attr)

    "v5"
    (if-let [current-value (get item attr)]
      (let [new-value (if (many-ref? current-value)
                        (map second current-value)
                        (second current-value))]
        (assoc item attr new-value))
      item)))

(defn clean-item-attrs
  [item attrs]
  (reduce (fn [i attr] (clean-attr i attr)) item attrs))

(defn clean-item
  [item]
  (reduce (fn [i attr] 
            (if (ref? (get item attr))
              (clean-attr i attr)
              i))
          item
          (keys item)))

(defn expand-attr
  "Expands an attribute by resolving the ref.
   Cleans expanded items so that they are suitable
   for returning in the response."
  [item attr]
  (if-let [ref (get item attr)]
    (let [resolved-item-or-items (resolve-ref ref)
          first-item (if (map? resolved-item-or-items) resolved-item-or-items (first resolved-item-or-items))
          attrs-to-clean (->> first-item
                              (map (fn [[k v]] (when (ref? v) k)))
                              (remove nil?))
          cleaned-item-or-items (if (map? resolved-item-or-items)
                                  (clean-item-attrs resolved-item-or-items attrs-to-clean)
                                  (map (fn [item] (clean-item-attrs item attrs-to-clean)) resolved-item-or-items))]
      (assoc item attr cleaned-item-or-items))
    item))

(defn expand-item-attrs
  [item attrs]
  (reduce (fn [i attr] (expand-attr i attr)) item attrs))

(defn vectorize
  [x]
  (if (coll? x)
    (vec x)
    [x]))

(defn expand-item
  [req item]
  (let [datatypes (vectorize (req->datatype req))
        expandable-attrs (req->expands req)
        attrs-to-expand (->> (get-in req [:query-params :expand])
                             (mapcat (fn [kw] (for [datatype datatypes]
                                                (combine-kw datatype kw))))
                             (set)
                             (intersection expandable-attrs))
        attrs-to-clean (difference expandable-attrs attrs-to-expand)] 
    (-> item
        (#(expand-item-attrs % attrs-to-expand))
        (#(clean-item-attrs % attrs-to-clean)))))

(defn many-handler
  [{:keys [ooapi-version] :as req}]
  (let [page-size (get-in req [:query-params :pageSize])
        page-number (get-in req [:query-params :pageNumber] 1)
        items (get-items req)
        filtered-items (->> items
                            (apply-select req)
                            (apply-filters req)
                            (map (partial expand-item req))
                            (map clean-item))
        total-pages (calc-total-pages page-size (count filtered-items))
        v5? (= ooapi-version "v5")]
    (cond-> {:pageSize page-size
             :pageNumber page-number
             :items (apply-pagination page-size page-number filtered-items)}
      v5? (assoc :hasPreviousPage (> 1 page-number)
                 :hasNextPage (< page-number total-pages)
                 :totalPages total-pages))))

(defn get-item-in-one
  [req]
  (let [id-path (req->id-path req)
        datatype (req->datatype req)
        id-attr (keyword (name datatype)
                         (name (last id-path)))
        id (req->id req)
        indexed-items (common/index-by id-attr (get-items req))]
    (get indexed-items id)))

(defn get-item-in-many
  [req]
  (let [id-path (req->id-path req)
        id (req->id req)
        datatypes (req->datatype req)]
    (first
     (remove nil?
             (for [datatype datatypes]
               (let [items (get data/data datatype)
                     id-attr (keyword (name datatype)
                                      (name (last id-path)))
                     indexed-items (common/index-by id-attr items)]
                 (get indexed-items id)))))))

(defn get-item
  [req]
  (let [datatype (req->datatype req)]
    (if (vector? datatype)
      (get-item-in-many req)
      (get-item-in-one req))))

(defn one-handler
  [req]
  (->> (get-item req)
       (expand-item req)
       (clean-item)))

(defn singleton-handler
  [req]
  (let [datatype (req->datatype req)]
    (first (get data/data datatype))))

(defn handler
  [req]
  (let [parsed-req (-> req coerce-query-parameters coerce-path-parameters)
        cardinality (req->cardinality parsed-req)
        result (case cardinality
                 :many (many-handler parsed-req)
                 :one (one-handler parsed-req)
                 :singleton (singleton-handler parsed-req)
                 :error)]
    (cond
      (= :error result) {:status 501
                         :body "Can't handle request"}
      (nil? result)     {:status 404
                         :body "Not found"}
      :else             {:status 200
                         :headers {"Content-Type" "application/json"}
                         :body (json/encode result {:key-fn name})})))

(defn create-chaos-handler
  [normal-handler modes]
  (fn [req]
    (let [mode (rand-nth modes)]
      (case mode
        :normal (normal-handler req)
        :empty {:status 200
                :headers {"Content-Type" "application/json"}
                :body ""}
        :slow (let [delay (+ 3000 (rand-int 60000))]
                (Thread/sleep delay)
                (normal-handler req))))))

(defn insert-ooapi-version
  [handler]
  (fn [req]
    (handler (assoc req :ooapi-version data/ooapi-version))))

(def parameters-middleware
  "Middleware to parse urlencoded parameters from the query string and form
  body (if the request is a url-encoded form). Adds the following keys to
  the request map:

  :query-params - a map of parameters from the query string
  :form-params  - a map of parameters from the body
  :params       - a merged map of all types of parameter"
  {:name ::parameters
   :compile (fn [{:keys [parameters]} _]
              (if (and (some? (:form parameters)) (nil? (:body parameters)))
                {:data {:swagger {:consumes ["application/x-www-form-urlencoded"]}}}
                {}))
   :wrap params/wrap-params})

(defn router
  []
  (ring/router
   (->> (data/routes)
        (mapv (juxt :path #(assoc % :get handler))))
   {:data {:middleware [insert-ooapi-version parameters-middleware]}}))

(defmethod ig/init-key ::app
  [_ {:keys [chaos? chaos-modes]}]
  (let [modes (->> (str/split chaos-modes #",")
                   (map str/lower-case)
                   (map keyword))
        normal-handler (ring/ring-handler (router)
                                          handler
                                          {:inject-match? true
                                           :inject-router? false})]
    (when chaos? (log/warn :chaos? true :msg "ChAoS mode is on!" :modes modes))
    (if chaos?
      (create-chaos-handler normal-handler modes)
      normal-handler)))

(comment

  (defn try-app
    ([uri query-string]
     (let [app (ring/ring-handler (router))
           request {:request-method :get
                    :uri uri
                    :query-string query-string}
           response-str (:body (app request))]
       (json/parse-string response-str)))
    ([uri]
     (try-app uri nil)))

  (try-app "/courses")

  (try-app "/courses" "pageSize=20")

  (try-app "/courses/fa00a62b-525b-19df-8a3c-47434a535c55" "expand=coordinators")

  (try-app "/education-specifications")

  ; no children 465b8146-59e1-a484-c72c-15b9d73e6f71
  (try-app "/education-specifications/465b8146-59e1-a484-c72c-15b9d73e6f71" "expand=children")

  ; with children fdc89678-1688-ce14-d649-6bcebac7f652
  (try-app "/education-specifications/fdc89678-1688-ce14-d649-6bcebac7f652" "expand=children")
  (try-app "/education-specifications/fdc89678-1688-ce14-d649-6bcebac7f652")

  ; with parent 7f4b5fab-3020-06b9-ac67-4e81352ceed1
  (try-app "/education-specifications/7f4b5fab-3020-06b9-ac67-4e81352ceed1" "expand=parent")
  (try-app "/education-specifications/7f4b5fab-3020-06b9-ac67-4e81352ceed1")

  ; no parent fdc89678-1688-ce14-d649-6bcebac7f652
  (try-app "/education-specifications/fdc89678-1688-ce14-d649-6bcebac7f652" "expand=parent")

  (try-app "/courses/a33cbd99-c437-3cee-d2e4-f5517958fd6c/offerings" #_"expand=parent")

  (try-app "/offerings/e4ddcd1b-c4b3-ff68-a21d-81e40b478c23" "expand=course")

  (->> data/data
       :educationSpecification
       (filter (comp #{#uuid "fdc89678-1688-ce14-d649-6bcebac7f652"} :educationSpecification/educationSpecificationId))
       first)

  (count (:course data/data))
  )