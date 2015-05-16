(ns sicp.helpers)

(defn error [& args]
  (throw (Exception. (apply str args))))

(defn tagged-list? [exp tag]
  (if (list? exp)
    (= (first exp) tag)
    tag))
