(ns sicp.ch5.fib-machine
  (:require [sicp.ch5.machine :refer [start
                                      get-register-contents
                                      set-register-contents!]]
            [sicp.ch5.assemble :refer [make-machine]]
            [clojure.test :refer :all]))

;; A double Recursion
(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def fib-controller
  '(controller
    (assign continue (label fib-done))

    fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answser))
    ;; set up to compute fib(n - 1)
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)                            ; save old value of n
    (assign n (op -) (reg n) (const 1)) ; clobber n to n - 1
    (goto (label fib-loop))             ; perform recursive call
    
    afterfib-n-1
    (restore n)
    (restore continue)
    ;; set up to comutpe fib(n - 2)
    (assign n (op -) (reg n ) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)                         ; save fib(n - 1)
    (goto (label fib-loop))

    afterfib-n-2
    (assign n (reg val))               ; upon return, val contains fib(n - 2)
    (restore val)                      ; n now contains fib(n - 2)
    (restore continue)
    (assign val (op + )                ; fib(n - 1) + fib(n - 2)
            (reg val) (reg n))
    (goto (reg continue))              ; return to caller, answer is in val

    immediate-answser
    (assign val (reg n))               ; base case: fib(n) = n
    (goto (reg continue))
    fib-done))

(defn make-fib-machine []
  (make-machine '(val n continue)
                {'- -, '+ +, '< <}
                fib-controller))

(deftest test-fib-machine
  (let [machine (make-fib-machine)]
    (set-register-contents! machine 'n 8)
    (start machine)
    (is (= 21 (get-register-contents machine 'val)))))


