(ns sicp.ch4.expr
  (:require [sicp.helpers :refer [tagged-list? error]]
            [clojure.test :refer :all]))

;; 这里的自求值表达式只有数和字符串
(defn self-evaluation? [exp]
  (cond (number? exp) true
        (string? exp) true
        (= exp   'true) true
        (= exp   'false) true
        :else false))

;; 变量用符号表示
(defn variable? [exp] (symbol? exp))

;; 引用表达式的形式是(quote <text-of-quotation>)
(defn quoted? [exp] (tagged-list? exp 'quote))
(defn text-of-quotation [exp] (second exp))

(defn quoted?           [[tag  _ :as exp]] (= tag 'quote))
(defn text-of-quotation [[_ text :as exp]] text)

;; 赋值的形式是(set! <var> <value>)
(defn assignment?         [exp] (tagged-list? exp 'set!))
(defn assignment-variable [exp] (second exp))
(defn assignment-value    [exp] (last exp))

;; lambda表达式是由符号lambda开始的表
(defn lambda?             [exp] (tagged-list? exp 'lambda))
(defn lambda-parameters   [exp] (second exp))
(defn lambda-body         [exp] (drop 2 exp))
(defn make-lambda         [parameters body]
  (list 'lambda parameters body))

;; 定义的形式是
;; (define <var> <value>)
;; 或者
;; (define (<var> (<parameter> ... <parameter>))
;;   <body>)
(defn definition?         [exp] (tagged-list? exp 'define))
(defn definition-variable [exp]
  (if (symbol? (second exp))
    (second exp)
    (first (second exp))))

(defn definition-value    [exp] (if (symbol? (second exp))
                                  (last exp)
                                  (let [[_ [_ & parameters] & body] exp]
                                    (make-lambda parameters body))))

;; 条件表达式由if开始，有一个谓词部分、一个推论部分和一个（可缺的）替代部分。
;; 如果这一表达式没有替代部分，我们就以false作为其替代。
(defn if?                 [exp] (tagged-list? exp 'if))
(defn if-predicate        [exp] (first exp))
(defn if-consequent       [exp] (second exp))
(defn if-alternative      [exp] (if (nth exp 3)
                                  (nth exp 3)
                                  nil))
(defn make-if             [predicate consequence alternative]
  (list 'if predicate consequence alternative))

;; begin包装起来一个表达式序列
(defn begin?              [exp] (tagged-list? exp 'begin))
(defn begin-actions       [exp] (rest exp))
(defn make-begin          [seq] (cons 'begin seq))
(defn sequence->exp       [seq] (case (count seq)
                                  0   nil
                                  1   (first seq)
                                  (make-begin seq)))

;; 过程应用就是不属于上述各种表达式类型的任意复合表达式
(defn application?        [exp] (list? exp))
(defn operator            [exp] (first exp))
(defn operands            [exp] (rest exp))
(defn no-operands?        [ops] (empty? ops))
(defn first-operands      [ops] (first ops))
(defn rest-operands       [ops] (rest ops))

;; 派生表达式cond
(defn cond?               [exp] (tagged-list? 'cond))
(defn cond-clauses        [exp] (rest exp))
(defn cond-predicate      [clause] (first clause))
(defn cond-actions        [clause] (rest clause))
(defn cond-else-clauses?  [clause] (= (cond-predicate clause) 'else))
(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false      ;; no else clause
    (let [first-clause (first clauses)
          rest-clauses (rest clauses)]
      (if (cond-else-clauses? first-clause)
        (if (empty? rest-clauses)
          (sequence->exp (cond-actions first-clause))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first-clause)
                 (sequence->exp (cond-actions first-clause))
                 (expand-clauses rest-clauses))))))
(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(deftest test-cond-expr
  (let [exp '(cond (1 2) (3 4) (else 5))
        expanded '(if 1 2
                      (if 3 4
                          5))]
    (is (= (cond->if exp)
           expanded))))
  
  
