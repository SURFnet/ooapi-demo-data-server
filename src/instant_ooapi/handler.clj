(ns instant-ooapi.handler
  (:require
    [cheshire.core :as json]
    [cheshire.generate :as jsong]
    [clojure.instant :refer [read-instant-date]]
    [clojure.string :as str]
    [clojure.set :refer [rename-keys]]
    [integrant.core :as ig]
    [io.pedestal.log :as log]
    [instant-ooapi.data :refer [data routes]]
    [instant-ooapi.common :as common]
    [reitit.ring :as ring]
    [reitit.ring.middleware.dev]
    [reitit.ring.middleware.parameters :refer [parameters-middleware]]))

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
      (reduce (fn [items dt] (concat items (get data dt)))
              [] datatype)
      (get data datatype))))

(jsong/add-encoder java.util.GregorianCalendar
                   (fn [c jsonGenerator]
                     (.writeString jsonGenerator (.format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssXXX") (.getTime c)))))

(defn is-ref?
  [v]
  (and (vector? v)
       (qualified-keyword? (first v))
       (uuid? (second v))))

(defn clean-item
  [item]
  (->> item
       (remove (fn [[_ v]] (is-ref? v)))
       (into {})))

(defn many-handler
  [req]
  (let [page-size (get-in req [:query-params :pageSize])
        page-number (get-in req [:query-params :pageNumber] 1)
        items (get-items req)]
    {:pageSize page-size
     :pageNumber page-number
     :items (->> items
                 (apply-select req)
                 (apply-filters req)
                 (apply-pagination page-size page-number)
                 (map clean-item))}))

(defn req->expands
  [req]
  (-> req ring/get-match :data :ooapi/expand))

(defn get-ref
  [[attr id]]
  (let [datatype (keyword (namespace attr))
        items (get data datatype)
        indexed-items (common/index-by attr items)]
    (get indexed-items id)))

(defn apply-expand
  [item {:keys [query-params] :as req}]
  (let [expands (req->expands req)
        to-expand (set (:expand query-params))
        expand (fn [item [exp [cardinality attrs rename]]]
                 (let [attr (if (vector? attrs) ; select the right attr by testing which are in the entity
                              (->> attrs (filter #(contains? item %)) first)
                              attrs)]
                   (if (contains? to-expand exp)
                     (let [ref (get item attr)
                           expanded-item (clean-item (get-ref ref))]
                       (cond-> item
                         true (assoc attr expanded-item)
                         (= cardinality :many) (assoc attr [expanded-item]) ; TODO this is a hack because we have no many-many relations between courses and programmes yet
                         rename (rename-keys {attr rename})))
                     (dissoc item attr))))]
    (reduce expand item expands)))

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
          (let [items (get data datatype)
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
  (-> (get-item req)
      (apply-expand req)))

(defn singleton-handler
  [req]
  (let [datatype (req->datatype req)]
    (first (get data datatype))))

(defn handler
  [req]
  (let [parsed-req (-> req coerce-query-parameters coerce-path-parameters)
        cardinality (req->cardinality parsed-req)
        result (case cardinality
                 :many (many-handler parsed-req)
                 :one (one-handler parsed-req)
                 :singleton (singleton-handler parsed-req)
                 {:status 501
                  :body "Not yet able to handle this request"})]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/encode result {:key-fn name})}))

(defn router
  []
  (ring/router
    (->> (routes)
         (mapv (juxt :path #(assoc % :get handler))))
    {:data {:middleware [parameters-middleware]}}))

(defmethod ig/init-key ::app
  [_ _]
  (ring/ring-handler (router)
                     handler
                     {:inject-match? true
                      :inject-router? false}))


;(app {:request-method :get :uri "/courses"
;      :query-string "pageNumber=2&level=bachelor&sort=name,-ects"})
