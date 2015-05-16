(ns sicp.ch5.machine
  (:refer-clojure :exclude [pop])
  (:require [sicp.helpers :refer [error]]
            [clojure.test :refer :all]))

;; register
(defn make-register [name]
  (let [contents (atom '*unassigned*)]
    (fn dispatch [message]
      (case message
        get @contents
        set (fn [value] (reset! contents value))
        (error "Unknown request -- REGISTER" message)))))

(defn get-contents  [register]       (register 'get))
(defn set-contents! [register value] ((register 'set) value))

(deftest test-register
  (let [register (make-register 'pc)]
    (set-contents! register 1)
    (is (= 1 (get-contents register)))))


;; stack
(defn- make-stack []
  (let [s (atom '())]
    (letfn [(push [x] (swap! s conj x))
            (pop  [] (if (empty? @s)
                        (error "Empty stack -- POP")
                        (let [top (first @s)]
                          (swap! s rest)
                          top)))
            (initialize [] (reset! s '())
              'done)
            (dispatch [message]
              (case message
                push push
                pop  (pop)
                (error ("Unknown request -- STACK" message))))]
      dispatch)))
(defn push [stack value] ((stack 'push) value))
(defn pop  [stack] (stack 'pop))

(deftest test-stack
  (let [stack (make-stack)]
    (push stack 1)
    (is (= 1 (pop stack)))))


;; instructions
(defn make-instruction [text] (atom [text]))
(defn instruction-text [inst] (first @inst))
(defn set-instruction-execution-proc! [inst proc] (swap! inst concat [proc]))
(defn instruction-execution-proc [inst]
  (if (symbol? inst)
    nil
    (second @inst)))

(deftest test-instruction
  (let [text "inc"
        proc inc]
    (let [inst (make-instruction text)]
      (set-instruction-execution-proc! inst proc)
      (is (= proc (instruction-execution-proc inst))))))

;; label
(defn make-label-entry [label-name insts]
  (cons label-name insts))

(defn lookup-label [labels label-name]
  (let [val (drop-while #(not=  (first %) label-name) labels)]
    (if (seq val)
      (rest (first val))
      (error "Undefined label -- ASSEMBLE" label-name))))


;; machine
(defn make-new-machine []
  (let [pc    (make-register 'pc)
        flag  (make-register 'flag)
        stack (make-stack)
        the-instruction-sequence (atom '())]
    (let [the-ops (atom (list (list 'initialize-stack
                                    (fn [] (stack 'initialzie)))))
          register-table (atom {'pc pc
                                'flag flag})]
      (letfn [
              (allocate-register [name]
                (if (contains? @register-table name)
                  (error "Multiply defined register: " name)
                  (swap! register-table assoc name (make-register name)))
                'register-allocated)
              
              (lookup-register [name]
                (let [val (@register-table name)]
                  (if (contains? @register-table name)
                    (@register-table name)
                    (error "未知的寄存器:" name))))
              
              (execute []
                (let [insts (get-contents pc)]
                  ;;(if (or (empty? insts) (symbol? (first insts)))
                  (if (empty? insts)
                    'done
                    (do
                      ((instruction-execution-proc (first insts)))
                      (execute)))))

              (dispatch [message]
                (case message
                  start
                  (do (set-contents! pc @the-instruction-sequence)
                      (execute))

                  install-instruction-sequence
                  (fn [seq] (reset! the-instruction-sequence seq))

                  allocate-register
                  allocate-register

                  get-register
                  lookup-register
                  
                  install-operations
                  (fn [ops] (swap! the-ops concat ops))

                  stack stack

                  operations the-ops

                  (error "Unknown request -- MACHINE" message)
                  ))]
        dispatch))))

(defn get-register [machine register-name]
  ((machine 'get-register) register-name))

;; pubic api

(defn start [machine] (machine 'start))

(defn get-register-contents [machine register-name]
  (get-contents (get-register machine register-name)))

(defn set-register-contents! [machine register-name value]
  (set-contents! (get-register machine register-name) value))
