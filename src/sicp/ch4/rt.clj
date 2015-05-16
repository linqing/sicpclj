(ns sicp.ch4.rt
  (:require [sicp.helpers :refer (tagged-list?)]))

(def apply-in-underlying-clojure apply)

(def ^:private primitive-procedures
  (list (list 'first first)
        (list 'rest rest)
        (list '+  +)))

(defn primitive-procedure-names [] (map first primitive-procedures))

(defn primitive-procedure-objects []
  (map (fn [proc]
         (list 'primitive (second proc)))
       primitive-procedures))

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation [proc]
  (second proc))

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-clojure
   (primitive-implementation proc) args))

;; compount procedure
(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
(defn compound-procedure?   [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters  [p] (nth p 1))
(defn procedure-body        [p] (nth p 2))
(defn procedure-environment [p] (nth p 3))
