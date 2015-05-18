(ns sicp.ch4.meval
  (:refer-clojure :exclude [eval apply])
  (:require [sicp.helpers :refer (error)]
            [sicp.ch4.expr :refer :all]
            [sicp.ch4.environment :as environ]
            [sicp.ch4.rt :refer :all]
            [clojure.test :refer :all]
            [clojure.core :as    clj]))

(declare eval apply the-global-environment)

(defn list-of-values [exps env]
  (map #(eval % env) exps))

(defn eval-if [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent  exp) env)
    (eval (if-alternative exp) env)))

(defn eval-sequence [exps env]
  (last
   (map #(eval % env) exps)))

(defn eval-definition [exp env]
  (environ/define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


(defn eval-assignment [exp env]
  (environ/set-variable-value! (assignment-variable exp)
                               (assignment-value exp)
                               env)
  'ok)

(defn eval [exp env]
  (condp clj/apply [exp]
    true?            true
    false?           false
    self-evaluation? exp
    variable?        (environ/lookup-variable-value exp env)
    quoted?          (text-of-quotation exp)
    assignment?      (eval-assignment exp env)
    definition?      (eval-definition exp env)
    if?              (eval-if exp env)
    lambda?          (make-procedure (lambda-parameters exp) (lambda-body exp) env)
    begin?           (eval-sequence (begin-actions exp) env)
    cond?            (eval (cond->if exp) env)
    application?     (apply (eval (operator exp) env)
                            (list-of-values (operands exp) env))
    (error "不能识别的表达式--EVAL:" exp)))

(defn apply [procedure arguments]
  (cond
    (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
    (compound-procedure? procedure)  (eval-sequence
                                      (procedure-body procedure)
                                      (environ/extend-environment
                                       (procedure-parameters procedure)
                                       arguments
                                       (procedure-environment procedure)))
    :else                            (error "Unknown procedure type -- APPLY" procedure)))

(defn setup-environment []
  (let [initial-env (->> environ/the-empty-environment
                         (environ/extend-environment (primitive-procedure-names)
                                                     (primitive-procedure-objects))
                         (environ/extend-environment '(pi) '(3.14)))]
    initial-env))

(def the-global-environment (setup-environment))

(deftest test-eval
  (let [env (setup-environment)]
    ;; self evaluation
    (is (= 1 (eval 1 env)))
    (is (= "hello" (eval "hello" env)))

    ;; variable lookup
    (is (= 3.14 (eval 'pi env)))

    ;; quoted
    (is (= '(a b c) (eval '(quote (a b c)) env)))

    ;; assignment
    (is (= 'ok (eval '(set! pi 6.28) env)))

    ;; definition
    (is (= 'ok (eval '(define a 1) env)))

    ;; if
    (is (= 1 (eval '(if true 1 2) env)))

    ;; lambda
    (is (compound-procedure?
         (eval '(lambda (x) (+ x 1)) env)))

    ;; begin
    (is (= 2 (eval '(begin 1 2) env)))
    
    ;; application
    (is (= 3 (eval '(+ 1 2) env)))))

(deftest test-program
  (let [env (setup-environment)]
    (eval '(define incc (lambda (x) (+ x 1))) env)
    (is (= 2 (eval '(incc 1) env)))))

