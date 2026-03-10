(ns ooapi-demo-data-server.fields-param
  (:require [clojure.walk :as walk]))

(defn- tokenize-fields-param
  [fields]
  (re-seq #"[(),]|[^(),]+" fields))

;; See "fields" at https://openonderwijsapi.nl/specification/v6.0/docs.html#tag/courses/operation/listCourseById

(defn- parse-fields-paths
  [fields]
  (when fields
    (let [tokens (re-seq #"[(),]|[^(),]+" fields)]
      (loop [paths []
             current-path []
             [token & tokens] tokens]
        (if token
          (case token
            ")" (recur paths (vec (drop-last current-path)) tokens)
            "," (recur paths current-path tokens)
            "(" (recur paths current-path tokens)
            (if (= "(" (first tokens))
              (recur paths (conj current-path token) tokens)
              (recur (conj paths (conj current-path token)) current-path tokens)))
          paths)))))

;; these attributes map to the names in the schema.json file
;;
;; i.e.
;;
;;  {
;;   "name": "organisation",
;;   "attributes": {
;;     "organisationId": {
;;       "generator": ["uuid"]
;;     }
;;  }
;;
;; maps to :organisation/organisationId

(def required?
  #{:organisation/organisationId
    :course/courseId
    :programme/programmeId
    :programme/programmeType
    :programme/name
    :programme/primaryCode})

(defn- select-required
  [item]
  (->> item
       (walk/postwalk (fn [x]
                        (if (map? x)
                          (reduce-kv (fn [m k v]
                                       (if (required? k)
                                         (assoc m (name k) v)
                                         m))
                                     {} x)
                          x)))))

(defn- copy-path
  [dest src [k & rest-path :as path]]
  (cond
    (empty? path)
    src

    (sequential? src)
    (mapv (fn [d s]
            (copy-path d s path))
          (or dest (repeat (count src) nil)) src)

    (map? src)
    (if (contains? src k)
      (update dest k #(copy-path % (get src k) rest-path))
      dest)))

(defn select-fields
  [{{:keys [fields]} :query-params} item]
  (let [paths (parse-fields-paths fields)
        ;; keep all fields; just stringify, stripping namespace from keys
        ;; note we need the original with qualified keys for "select-required"
        stringified (walk/stringify-keys item)]
    (if (seq paths)
      (reduce (fn [m path]
                (copy-path m stringified path))
              (select-required item)
              paths)
      stringified)))
