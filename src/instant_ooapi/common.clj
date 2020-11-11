(ns instant-ooapi.common)

; Copied from ghadi on Clojurians Slack
(defn index-by
  "Builds a map whose entries are the result of applying
   keyf and valf to every item in the provided collection.
   Throws an exception if two items map to the same key,
   unless mergef is provided.

   valf defaults to identity"
  ([keyf coll]
   (index-by keyf identity coll))
  ([keyf valf coll]
   (reduce (fn [m v]
             (let [k (keyf v)]
               (if (find m k)
                 (throw (ex-info "Duplicate key" {:k k}))
                 (assoc m k (valf v)))))
           {}
           coll))
  ([keyf valf mergef coll]
   (reduce (fn [m v]
             (let [k (keyf v)]
               (if-let [entry (find m k)]
                 (assoc m k (mergef (val entry) (valf v)))
                 (assoc m k (valf v)))))
           {}
           coll)))