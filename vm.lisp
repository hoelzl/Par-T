;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Based on code from Paradigms of Artificial Intelligence
;;; Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Bug fixes by Erann Gat, gat@aig.Jpl.Nasa.Gov, November 1992
;;; Modifications Copyright (c) 2012 Matthias Hölzl

;;; The modifications to this file are licensed under the MIT license;
;;; see the file LICENSE in the root directory for further
;;; information.

;;;; Simple VM for Par-T.

(in-package :parallel-thetis)

(defstruct ret-addr fn pc env)

(defun top (stack) (first stack))

(defvar *trace-par-t-vm* nil)
(defvar *trace-par-t-global-loads* nil)

;;; Support for threads
;;; ===================

(defstruct (thread-state)
  (fun nil :type (or fn pt-entity))
  (code nil)
  (pc 0 :type (integer 0))
  (env '())
  (stack '())
  (n-args 0 :type (integer 0))
  (instr nil))

(defstruct (thread (:constructor %make-thread (id state priority weight)))
  (id (gensym "THREAD-"))
  (state)
  (priority)
  ;; Weight is initially the priority, but can be changed by the scheduler for
  ;; to implement scheduling strategies.
  (weight))

(defun make-thread (&key (id (gensym "THREAD-"))
                         (state nil)
                         (priority nil))
  (%make-thread id state priority priority))


;;; Utility Functions
;;; =================

(defun set-up-call (state fun call-n-args)
  (cond ((fn-p fun)
         (setf (thread-state-fun state) fun
               (thread-state-code state) (fn-code fun)
               (thread-state-pc state) 0
               (thread-state-env state) (fn-env fun)
               (thread-state-n-args state) call-n-args)
         (values t nil nil 'entity-call))
        ((pt-entity-p fun)
         (let ((real-fun (%instance-proc fun)))
           (cond ((not (fn-p real-fun))
                  (values nil 
                          t 
                          nil 
                          (list 'bad-entity-call fun real-fun)))
                 (t
                  (setf (thread-state-fun state) real-fun
                        (thread-state-code state) (fn-code real-fun)
                        (thread-state-pc state) 0
                        (thread-state-env state) (fn-env real-fun)
                        (thread-state-n-args state) call-n-args)
                  (values t nil nil 'entity-call)))))
        (t
         (values nil t nil (list 'bad-function-call fun)))))

(defun print-trace-information (state)
  (when *trace-par-t-vm*
    (format *trace-output* "~&Starting VM iteration:~%")
    (format *trace-output* "  Function:~15T~A~%"
            (fn-name (thread-state-fun state)))
    (format *trace-output* "  Program Counter:~15T~D~%"
            (thread-state-pc state))
    (format *trace-output* "  Instruction:~15T~:W~%"
            (thread-state-instr state))
    (format *trace-output* "  Stack:~15T~:W~%"
            (thread-state-stack state))
    (unless (eq *trace-par-t-vm* :short)
      (format *trace-output* "  Code:~15T~:W~%"
              (thread-state-code state))
      (format *trace-output* "  Environment:~15T~:W~%"
              (thread-state-env state))
      (format *trace-output* "  Number of Args:~15T~D~%"
              (thread-state-n-args state)))))

;;; One-Step Execution
;;; ==================

(defun run-thread-1-step (state locale)
  "Runs the thread with state STATE for a single step using LOCALE.
Returns four values:

  * continue?: 
    true if execution can continue after the current instruction,
    false if this thread cannot continue

  * error?:
    true if there was an error while executing the current
    instruction (this implies that continue? is false),
    false otherwise

  * value:
    the return value of the instruction if both continue? and
    error? are false, undefined otherwise

  * reason:
    a reason for the error, or the opcode of the instruction that was
    executed if there was no error"
  (let* ((instr (thread-state-instr state))
         (opcode (opcode instr)))
    (case opcode
      ;; Variable/stack manipulation instructions:
      (LVAR
       (let* ((instr (thread-state-instr state))
              (env (thread-state-env state))
              (env-frame (elt env (arg1 instr))))
         (push (elt env-frame (arg2 instr))
               (thread-state-stack state)))
       (values t nil nil 'lvar))
      (LSET
       (let* ((instr (thread-state-instr state))
              (env (thread-state-env state))
              (env-frame (elt env (arg1 instr))))
         (setf (elt env-frame (arg2 instr))
               (top (thread-state-stack state))))
       (values t nil nil 'lset))
      (GVAR 
       (let ((var (arg1 (thread-state-instr state))))
         (multiple-value-bind (value presentp)
             (get-var-in-locale var locale)
           (cond (presentp
                  (when *trace-par-t-global-loads*
                    (format *trace-output*
                            "~&>>> PT: accessing ~A~%" (arg1 (thread-state-instr state))))
                  (push value (thread-state-stack state))
                  (values t nil nil 'gvar))
                 (t
                  (values nil t nil (list 'unbound-variable var)))))))
      (GSET
       (setf (get-var-in-locale (arg1 (thread-state-instr state)) locale)
             (top (thread-state-stack state)))
       (values t nil nil 'gset))
      (POP
       (pop (thread-state-stack state))
       (values t nil nil 'pop))
      (CONST
       (push (arg1 (thread-state-instr state)) (thread-state-stack state))
       (values t nil nil 'cost))
      
      ;; Branching instructions:
      (JUMP
       (setf (thread-state-pc state) (arg1 (thread-state-instr state)))
       (values t nil nil 'jump))
      (FJUMP
       (when (eq *false* (pop (thread-state-stack state)))
         (setf (thread-state-pc state) (arg1 (thread-state-instr state))))
       (values t nil nil 'fjump))
      (TJUMP
       (when (not (eq *false* (pop (thread-state-stack state))))
         (setf (thread-state-pc state) (arg1 (thread-state-instr state))))
       (values t nil nil 'tjump))
      
      ;; Function call/return instructions:
      (SAVE
       (push (make-ret-addr :pc (arg1 (thread-state-instr state))
                            :fn (thread-state-fun state)
                            :env (thread-state-env state))
             (thread-state-stack state))
       (values t nil nil 'save))
      (RETURN
        ;; The return value is the top of the stack; the return-address is
        ;; second if we don't return from the top level.
        (let* ((stack (thread-state-stack state))
               (ret-addr (second stack)))
          (cond ((and ret-addr (ret-addr-p ret-addr))
                 (let ((fun (ret-addr-fn ret-addr)))
                   (setf (thread-state-fun state) fun
                         (thread-state-code state) (fn-code fun)
                         (thread-state-pc state) (ret-addr-pc ret-addr)
                         (thread-state-env state) (ret-addr-env ret-addr)))
                 ;; Get rid of the return-address, but keep the value on the
                 ;; stack.
                 (setf (thread-state-stack state) (cons (first stack) (rest2 stack)))
                 (values t nil nil 'return))
                (t
                 ;; We are trying to return from the top level. This happens
                 ;; in particular when call/cc captures the outermost
                 ;; continuation, since there is no return address (and no
                 ;; value) on the stack.  It's not quite clear whether we
                 ;; should classify this as an error or a normal way to halt
                 ;; execution, but since we are currently using it in the
                 ;; REPL, let's treat it as a normal return for now.
                 
                 ;; (warn "Returning from top level?")
                 (values nil nil (first stack) 'top-level-return)))))
      
      (CALLJ
       ;; Set the active function to the function object on the stack.
       (let ((fun (pop (thread-state-stack state))))
         (set-up-call state fun (arg1 (thread-state-instr state)))))
      (CALLJ-VARARGS
       ;; Set the active function to the function object on the stack and the
       ;; number of arguments to the second object on the stack.
       (let* ((fun (pop (thread-state-stack state)))
              (n-args (pop (thread-state-stack state))))
         (set-up-call state fun n-args)))
      
      (ARGS
       (let* ((instr (thread-state-instr state))
              (n-req (arg1 instr))
              (n-args (thread-state-n-args state)))
         (cond ((not (= n-args n-req))
                (values nil t nil (list 'bad-args n-req n-args)))
               (t
                (let ((new-bindings (make-array n-args)))
                  (push new-bindings (thread-state-env state))
                  (loop for i from (- n-args 1) downto 0 do
                           (setf (elt new-bindings i)
                                 (pop (thread-state-stack state)))))
                (values t nil nil 'args)))))
      (VARARGS
       (let* ((instr (thread-state-instr state))
              (n-req (arg1 instr))
              (n-args (thread-state-n-args state)))
         (cond ((< n-args n-req)
                (values nil t nil (list 'bad-varargs n-req n-args)))
               (t
                (let ((new-bindings (make-array (+ 1 n-req) :initial-element '())))
                  (push new-bindings (thread-state-env state))
                  (loop repeat (- n-args n-req) do
                           (push (pop (thread-state-stack state))
                                 (elt new-bindings n-req)))
                  (loop for i from (- n-req 1) downto 0 do
                           (setf (elt new-bindings i)
                                 (pop (thread-state-stack state)))))
                (values t nil nil 'varargs)))))
      
      (FN
       (push (make-fn :code (fn-code (arg1 (thread-state-instr state)))
                      :name (fn-name (arg1 (thread-state-instr state)))
                      :args (fn-args (arg1 (thread-state-instr state)))
                      :env (thread-state-env state)) 
             (thread-state-stack state))
       (values t nil nil 'fn))
      
      (LISP-CALL
       (let ((fun (fdefinition (pop (thread-state-stack state))))
             (args (pop (thread-state-stack state))))
         (push (apply fun args) (thread-state-stack state)))
       (values t nil nil 'lisp-call))
      
      (PRIM
       (let ((fun (fdefinition (arg1 (thread-state-instr state))))
             (n-args (thread-state-n-args state))
             (args '()))
         (dotimes (i n-args)
           (push (pop (thread-state-stack state)) args))
         (push (apply fun args) (thread-state-stack state)))
       (values t nil nil 'prim))
      
      ;; Continuation instructions:
      (SET-CC
       (setf (thread-state-stack state)
             (top (thread-state-stack state)))
       (values t nil nil 'set-cc))
      (CC
       (push (make-fn
              :env (list (vector (thread-state-stack state)))
              :name '%%primitive-cc
              :code '((ARGS 1)
                      (LVAR 1 0 ";"  state)
                      (SET-CC)
                      (LVAR 0 0)
                      (RETURN)))
             (thread-state-stack state))
       (values t nil nil 'cc))
      
      ;; Nullary operations:
      #+(or)
      ((PAR-T-READ NEWLINE) 
       (push (funcall (opcode (thread-state-instr state))) (thread-state-stack state)))
      
      ;; Unary operations:
      ((CAR CDR CADR PAR-T-NOT PAR-T-NULL
            PAR-T-CONSP PAR-T-BOOLEANP PAR-T-SYMBOLP PAR-T-FN-P 
            PAR-T-NUMBERP PAR-T-VECTORP PAR-T-CHARACTERP PAR-T-STRINGP
            %INSTANCE-CLASS %INSTANCE-PROC %INSTANCEP
            COMPILER) 
       (push (funcall opcode (pop (thread-state-stack state)))
             (thread-state-stack state))
       (values t nil nil opcode))
      
      ;; Binary operations:
      ((+ - * / PAR-T-< PAR-T-> PAR-T-<= PAR-T->= PAR-T-/= PAR-T-=
          CONS CAR-SETTER CDR-SETTER
          %ALLOCATE-INSTANCE %ALLOCATE-ENTITY
          %INSTANCE-REF
          NAME! PAR-T-EQ PAR-T-EQUAL PAR-T-EQL)
       (let ((stack (thread-state-stack state)))
         (setf (thread-state-stack state)
               (cons (funcall opcode (second stack) (first stack))
                     (rest2 (thread-state-stack state)))))
       (values t nil nil opcode))
      
      ;; Ternary operations:
      ((%INSTANCE-SETTER %INSTANCE-PROC-SETTER)
       (let ((stack (thread-state-stack state))
             (opcode (opcode (thread-state-instr state))))
         (setf (thread-state-stack state)
               (cons (funcall opcode
                              (third stack) (second stack) (first stack))
                     (rest3 (thread-state-stack state)))))
       (values t nil nil opcode))
      
      ;; Constants:
      ((PAR-T-TRUE)
       (push *true* (thread-state-stack state))
       (values t nil nil 'true))

      ((PAR-T-FALSE)
       (push *false* (thread-state-stack state))
       (values t nil nil 'false))

      ((-1 0 1 2)
       (push (opcode (thread-state-instr state)) (thread-state-stack state))
       (values t nil nil opcode))
      
      ;; Other:
      ((HALT)
       (values nil nil (top (thread-state-stack state)) 'halt))
      
      (otherwise
       (values nil t nil 'unknown-opcode)))))


;;; Running a Single Thread 
;;; =======================

(defvar *default-thread-time-slice* most-positive-fixnum)

(defun run-thread (state &key (locale (top-level-locale))
                              (ticks *default-thread-time-slice*))
  "Run the abstract machine on the code for f."
  (check-type state thread-state)
  ;;; TODO: Move these to the top-level
  (dotimes (tick ticks (values :time-slice-exhausted nil))
    (setf (thread-state-instr state)
          (elt (thread-state-code state) (thread-state-pc state)))
    (print-trace-information state)
    (incf (thread-state-pc state))
    (multiple-value-bind (continue? error? value reason)
        (run-thread-1-step state locale)
      (unless continue?
        (if error?
            (error "Runtime error: ~A~%  State: ~:W" reason state)
            (return-from run-thread (values :done value)))))))

(defgeneric run (executable &key locale)
  (:method ((fn fn) &key (locale (top-level-locale)))
    (run (make-thread-state :fun fn
                            :code (fn-code fn))
         :locale locale))

  (:method ((thread thread) &key (locale (top-level-locale)))
    (run (thread-state thread) :locale locale))

  (:method ((state thread-state) &key (locale (top-level-locale)))
    (multiple-value-bind (exit-status value)
        (run-thread state :locale locale)
      (if (eq exit-status :time-slice-exhausted)
          (run state :locale locale)
          value))))
