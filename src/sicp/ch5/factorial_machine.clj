(ns sicp.ch5.factorial-machine
  (:require [sicp.ch5.machine :refer [start
                                      get-register-contents
                                      set-register-contents!]]
            [sicp.ch5.assemble :refer [make-machine]]
            [clojure.test :refer :all]))

;; 
(defn factorial [n]
  (if (= n 1)
    1
    (* (factorial (- n 1))
       n)))

(def factorial-controller
  '(controller
    (assign continue (label fact-done))
    
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; set up for the recursive call by saving n and continue.
    ;; set up continue so that the computation will continue
    ;; at ater-fact when the subroutine returns.
    (save continue)
    (save n)

    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))

    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))

    base-case
    (assign val (const 1))
    (goto (reg continue))
    fact-done))

(defn make-factorial-machine []
  (make-machine '(val n continue)
                {'* *
                 '- -
                 '= =}
                factorial-controller))

(deftest test-factorial-machine []
  (let [machine (make-factorial-machine)]
    (set-register-contents! machine 'n 5)
    (start machine)
    (is (= 120 (get-register-contents machine 'val)))))
