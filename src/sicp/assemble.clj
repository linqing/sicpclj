(ns sicp.ch5.assemble
  (:refer-clojure :exclude [push pop])
  (:require [sicp.ch5.machine :refer :all]
            [clojure.test :refer :all]
            [sicp.helpers :refer [tagged-list? error]]))

;; ========== 子表达式的执行过程 ==========
(defn register-exp?      [exp] (tagged-list? exp 'reg))
(defn register-exp-reg   [exp] (second exp))

(defn constant-exp?      [exp] (tagged-list? exp 'const))
(defn constant-exp-value [exp] (second exp))

(defn label-exp?         [exp]  (tagged-list? exp 'label))
(defn label-exp-label    [exp]  (second exp))

(defn make-primitive-exp [exp machine labels]
  (cond
    (constant-exp? exp)
    (let [c (constant-exp-value exp)]
      (constantly c))

    (label-exp? exp)
    (let [insts (lookup-label labels
                              (label-exp-label exp))]
      (constantly insts))

    (register-exp? exp)
    (let [r (get-register machine (register-exp-reg exp))]
      #(get-contents r))

    :else
    (error "Unknown expression type -- ASSEMBLE" exp)))


(defn- operation-exp?  [exp] (and (sequential? exp) (tagged-list? (first exp) 'op)))
(defn- operation-exp-op [operation-exp] (second (first operation-exp)))
(defn- operation-exp-operands [operation-exp] (rest operation-exp))
(defn- lookup-prim [symbol operations]
  (let [val (first (filter #(= symbol (first %)) @operations))]
    (if val
      (second val)
      (error "Unknown operation -- ASSEMBLE" symbol))))

(defn make-operation-exp [exp machine labels operations]
  (let [op (lookup-prim (operation-exp-op exp) operations)
        aprocs (map (fn [e]
                      (make-primitive-exp e machine labels))
                    (operation-exp-operands exp))]
    #(apply op (map (fn [p] (p)) aprocs))))

(defn advance-pc [pc]
  (set-contents! pc (rest (get-contents pc))))

;; assign
(defn- assign-reg-name  [assign-instruction] (second assign-instruction))
(defn- assign-value-exp [assign-instruction] (drop 2 assign-instruction))
(defn make-assign [inst machine labels operations pc]
  (let [target (get-register machine (assign-reg-name inst))
        value-exp (assign-value-exp inst)]
    (let [value-proc (if (operation-exp? value-exp)
                       (make-operation-exp value-exp machine labels operations)
                       (make-primitive-exp (first value-exp) machine labels))]
      #(do (set-contents! target (value-proc))
           (advance-pc pc)))))

(deftest test-make-design []
  (let [assign-1 '(assign a (reg b))
        assign-2 '(assign a (const 1))
        assign-3 '(assign a (op inc) 1)]))

;; perform
(defn- perform-action [inst] (rest inst))
(defn make-perform [inst machine labels operations pc]
  (let [action (perform-action inst)]
    (if (operation-exp? action)
      (let [action-proc (make-operation-exp action machine labels operations)]
        #(do (action-proc)
             (advance-pc pc)))
      (error "Bad PERFORM instruction: ASSEMBLE" inst))))
(deftest test-make-perform []
  (let [perform-1 '(perform (op inc) 1)]
    ))

;; test
(defn- test-condition [test-instruction] (rest test-instruction))

(defn make-test [inst machine labels operations flag pc]
  (let [condition (test-condition inst)]
    (if (operation-exp? condition)
      (let [condition-proc (make-operation-exp condition machine labels operations)]
        #(do (set-contents! flag (condition-proc))
             (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))

(deftest test-make-test []
  (let [test-1 '(test (op gt) 1 2)]
    ))

;; branch
(defn- branch-test [branch-instruction] (second branch-instruction))
(defn make-branch [inst machine labels flag pc]
  (let [dest (branch-test inst)]
    (if (label-exp? dest)
      (let [insts (lookup-label labels (label-exp-label dest))]
        #(do (if (get-contents flag)
               (set-contents! pc insts)
               (advance-pc pc))))
      (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(deftest test-make-branch []
  (let [brach-1 '(label start)]))

;; goto
(defn- goto-dest [goto-instruction] (second goto-instruction))
(defn make-goto [inst machine labels pc]
  (let [dest (goto-dest inst)]
    (cond
      (label-exp? dest)
      (let [insts (lookup-label labels (label-exp-label dest))]
        #(set-contents! pc insts))

      (register-exp? dest)
      (let [reg (get-register machine dest)]
        #(set-contents! pc (get-contents reg)))

      :else
      (error "Bad GOTO instruction -- ASSEMBLE" inst))))

(deftest test-make-goto []
  (let [goto-1 '(goto (reg a))]
    ))

;; save
(defn- stack-inst-reg-name [stack-instruction] (second stack-instruction))
(defn make-save [inst machine stack pc]
  (let [reg (get-register machine (stack-inst-reg-name inst))]
    #(do (push stack (get-contents reg))
         (advance-pc pc))))

(deftest test-make-save []
  (let [save-1 '(save a)]))

;;restore
(defn make-restore [inst machine stack pc]
  (let [reg (get-register machine
                          (stack-inst-reg-name inst))]
    #(do (set-contents! reg (pop stack))
        (advance-pc pc))))

(deftest test-make-restore
  (let [store-1 '(restore a)]
    ))

(defn make-execution-procedure [inst labels machine pc flag stack ops]
  (case (first inst)
    assign  (make-assign  inst machine labels ops pc)
    test    (make-test    inst machine labels ops flag pc)
    branch  (make-branch  inst machine labels flag pc)
    goto    (make-goto    inst machine labels pc)
    save    (make-save    inst machine stack  pc)
    restore (make-restore inst machine stack  pc)
    perform (make-perform inst machine labels ops pc)
    (error "Unknown instruction type -- ASSEMBLE" inst)))

(defn- update-insts!
  "修改指令表，原来这里只包含指令的正文，现在要加入对应的执行过程。"
  [insts labels machine]
  (let [pc (get-register machine 'pc)
        flag (get-register machine 'flag)
        stack (machine 'stack)
        ops (machine 'operations)]
    (doall (for [inst insts]
             (do
               (set-instruction-execution-proc!
                inst
                (make-execution-procedure
                 (instruction-text inst) labels machine
                 pc flag stack ops)))))))

;;
(defn extract-labels
  ""
  [text receive]
  (if (empty? text)
    (receive '() '())
    (extract-labels (rest text)
                    (fn [insts labels]
                      (let [next-inst (first text)]
                        (if (symbol? next-inst)
                          (receive insts
                                   (cons (make-label-entry next-inst insts)
                                         labels))
                          (receive (cons (make-instruction next-inst)
                                         insts)
                                   labels)))))))
(defn assemble
  "输入：控制器正文，机器模型
  输出：指令序列"
  [controller-text machine]
  (extract-labels controller-text
                  (fn [insts labels]
                    (update-insts! insts labels machine)
                    insts)))

(defn make-machine [register-names ops controller-text]
  (let [machine (make-new-machine)]
    (dorun (for [register-name register-names]
             ((machine 'allocate-register) register-name)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
