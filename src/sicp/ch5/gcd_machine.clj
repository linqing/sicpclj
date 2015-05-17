(ns sicp.ch5.gcd-machine
  (:require [sicp.ch5.machine :refer [start
                                      get-register-contents
                                      set-register-contents!]]
            [sicp.ch5.assemble :refer [make-machine]]
            [clojure.test :refer :all]))

(deftest simple-machine []
  (let [machine (make-machine '(a b c)
                              '()
                              '(
                                test-b
                                (goto (label second))
                                first
                                (assign b (reg c))
                                second
                                (assign a (reg b))))]
    (set-register-contents! machine 'c 1)
    (start machine)
    (is (= 1 (get-register-contents machine 'a)))))

(defn make-gcd-machine []
  (make-machine
   '(a b t)
   {'rem rem
    '=   =}
   '(
     test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(deftest run-gcd-machine []
  (let [gcd-machine (make-gcd-machine)]
    (set-register-contents! gcd-machine 'a 206)
    (set-register-contents! gcd-machine 'b 40)
    (start gcd-machine)
    (is (= 2 (get-register-contents gcd-machine 'a)))))

(defn make-infinite-gcd-machone []
  (make-machine '(a b t)
                {'rem rem, '=   =}
                '(gcd-loop
                  (assign a (op read))
                  (assign b (op read))
                  test-b
                  (test (op =) (reg b) (const 0))
                  (branch (label gcd-done))
                  (assign t (op rem) (reg a) (reg b))
                  (assign a (reg b))
                  (assign b (reg t))
                  (goto (label test-b))
                  gcb-done
                  (perform (op print) (reg a))
                  (goto (label gcd-loop)))))
