(ns sicp.ch4.environment
  (:require [sicp.helpers :refer (error)]
   [clojure.test :refer :all]))

(defn- enclosing-environment [env] (rest env))
(defn- first-frame           [env] (first env))
(def the-empty-environment '())

(defn- make-frame [variables values]
  (list (atom variables) (atom values)))
(defn- frame-variables [frame] @(first frame))
(defn- frame-values    [frame] @(second frame))
(defn- frame-variables-atom [frame] (first frame))
(defn- frame-values-atom    [frame] (second frame))
(defn- frame-variables-contains [var env] (contains? (set (frame-variables env)) var))
(defn add-binding-to-frame! [var val frame]
  (swap! (first frame) conj var)
  (swap! (second frame ) conj val))
(defn alter-binding-to-frame! [var val frame]
  (let [new-vals (map (fn [var' val']
                        (if (= var' var) val val'))
                      (frame-variables frame)
                      (frame-values frame))]
    (reset! (frame-values-atom frame) new-vals)))

;; public
(defn lookup-variable-value
  "返回符号<var>在环境<env>中的约束值. 如果这一变量没有约束就发出一个错误信号"
  [var env]
  (letfn [(env-loop [env]
            (letfn [(scan [vars vals]
                      (cond
                        (empty? vars) (env-loop (enclosing-environment env))
                        (= var (first vars)) (first vals)
                        :else (recur (rest vars) (rest vals))))]
              (if (= env the-empty-environment)
                (error "Unbound variable: <" var ">")
                (let [frame (first-frame env)]
                  (scan (frame-variables frame)
                        (frame-values frame))))))]
    (env-loop env)))

(defn extend-environment
  "返回一个新环境，这个环境中包含了一个新的框架, 其中所有位于表<vars>里的符号约束到表<vals>里对应的元素，而
其外围环境是环境<base-env>"
  [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied"  vars vals))))

(defn define-variable!
  "在环境<env>的第一个框架里加入一个新约束，它关联起变量<var>和值<value>"
  [var value env]
  (let [frame (first-frame env)]
    (if (frame-variables-contains var frame)
      (alter-binding-to-frame! var value frame)
      (add-binding-to-frame! var value frame))))

(defn set-variable-value!
  "修改变量<var>在环境<env>里的约束，使得该变量现在约束到值<value>。如果这一变量没有约束就发出一个错误信号。"
  [var value env]
  (if (= env the-empty-environment)
    (error "Unbound variable: SET!")
    (if (frame-variables-contains var (first-frame env))
      (alter-binding-to-frame! var value (first-frame env))
      (recur var value (enclosing-environment env)))))


(deftest test-environment []
  ;; frame
  (let [frame (make-frame '(a b c) '(1 2 3))]
    (is (= '(a b c) (frame-variables frame)))
    (is (= '(1 2 3) (frame-values frame))))

  (let [env (extend-environment '(a b c) '(1 2 3) the-empty-environment)]
    (is (= 2 (lookup-variable-value 'b env)))))
