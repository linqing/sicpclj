(ns sicp.ch4.meval
  (:require [sicp.helpers :refer (error)]
            [sicp.ch4.expr :as expr]
            [sicp.ch4.environment :as environ]
            [sicp.ch4.rt :as rt]
            [clojure.test :refer :all]))

(declare meval mapply the-global-environment)


(defn eval-definition [exp env]
  (environ/define-variable!
    (expr/definition-variable exp)
    (meval (expr/definition-value exp) env)
    env)
  'ok)

(defn eval-assignment [exp env]
  (environ/set-variable-value! (expr/assignment-variable exp)
                               (expr/assignment-value exp)
                               env)
  'ok)

(defn eval-if [exp env]
  (if (true? (meval (expr/if-predicate exp) env))
    (meval (expr/if-consequent exp) env)
    (meval (expr/if-alternative exp) env)))

(defn eval-sequence [exps env]
  (cond
    (expr/last-exp? exps)
    (meval (expr/first-exp exps) env)

    :else
    (do
      (meval (expr/first-exp exps) env)
      (eval-sequence (expr/rest-exps exps) env))))

(declare meval)
(defn list-of-values [exps env]
  (map #(meval % env) exps))

(defn meval [exp env]
  (cond
    (= exp true) true

    (= exp false) false
    
    (expr/self-evaluation? exp)
    exp

    (expr/variable? exp)
    (environ/lookup-variable-value exp env)

    (expr/quoted? exp)
    (expr/text-of-quotation exp)

    (expr/assignment? exp)
    (eval-assignment exp env)

    (expr/definition? exp)
    (eval-definition exp env)

    (expr/if? exp)
    (eval-if exp env)

    (expr/lambda? exp)
    (rt/make-procedure (expr/lambda-parameters exp)
                       (expr/lambda-body exp)
                       env)

    (expr/begin? exp)
    (eval-sequence (expr/begin-actions exp) env)

    (comment expr/cond? exp)
    (comment meval (expr/cond->if exp) e)
    
    (expr/application? exp)
    (mapply (meval (expr/operator exp) env)
            (list-of-values
               (expr/operands exp) env))

    :else
    (error "不能识别的表达式--EVAL:" exp)))

(defn mapply [procedure arguments]
  (cond
    (rt/primitive-procedure? procedure)
    (rt/apply-primitive-procedure procedure arguments)
    
    (rt/compound-procedure? procedure)
    (eval-sequence
       (rt/procedure-body procedure)
       (environ/extend-environment
        (rt/procedure-parameters procedure)
        arguments
        (rt/procedure-environment procedure)))
    
    :else
    (error "Unknown procedure type -- APPLY" procedure)))

(defn setup-environment []
  (let [initial-env (->> environ/the-empty-environment
                         (environ/extend-environment (rt/primitive-procedure-names)
                                                     (rt/primitive-procedure-objects))
                         (environ/extend-environment '(pi) '(3.14)))]
    initial-env))

(def the-global-environment (setup-environment))

(deftest test-meval
  (let [env (setup-environment)]
    ;; self evaluation
    (is (= 1 (meval 1 env)))
    (is (= "hello" (meval "hello" env)))

    ;; variable lookup
    (is (= 3.14 (meval 'pi env)))

    ;; quoted
    (is (= '(a b c) (meval '(quote (a b c)) env)))

    ;; assignment
    (is (= 'ok (meval '(set! pi 6.28) env)))

    ;; definition
    (is (= 'ok (meval '(define a 1) env)))

    ;; if
    (is (= 1 (meval '(if true 1 2) env)))

    ;; lambda
    (is (rt/compound-procedure?
         (meval '(lambda (x) (+ x 1)) env)))

    ;; begin
    (is (= 2 (meval '(begin 1 2) env)))
    
    ;; application
    (is (= 3 (meval '(+ 1 2) env)))))

(deftest test-program
  (let [env (setup-environment)]
    (meval '(define incc (lambda (x) (+ x 1))) env)
    (is (= 2 (meval '(incc 1) env)))))

