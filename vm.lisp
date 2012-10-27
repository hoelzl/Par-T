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

;;; This is the function `(lambda () (exit))', with the name
;;; `the-default-error-function', compiled manually.
(defparameter *error-fun* 
  (make-fn :code '((args 0) (gvar exit) (callj 0))
	   :env nil
	   :name 'the-default-error-function
	   :args '()))

(defstruct ret-addr fn pc env)

(defun top (stack) (first stack))

(defvar *trace-par-t-vm* nil)
(defvar *trace-par-t-global-loads* nil)

(defstruct (vm-state)
  (fun nil :type (or fn pt-entity))
  (error-fun nil :type (or fn pt-entity))
  (code nil)
  (pc 0 :type (integer 0))
  (env '())
  (stack '())
  (n-args 0 :type (integer 0))
  (instr nil))

(defun machine (fun-or-state &key (locale (top-level-locale)))
  "Run the abstract machine on the code for f."
  ;;; TODO: Move these to the top-level
  (let ((state
          (ctypecase fun-or-state
            (fn (make-vm-state :fun fun-or-state
                               :code (fn-code fun-or-state)
                               :error-fun fun-or-state))
            (vm-state fun-or-state))))
    (labels ((machine-error (format-string &rest args)
               (apply #'cerror 
                      "Restart the computation with the error function."
                      format-string args)
               (setf (vm-state-fun state) (vm-state-error-fun state)
                     (vm-state-code state) (fn-code (vm-state-error-fun state))
                     (vm-state-pc state) 0
                     (vm-state-env state) nil
                     (vm-state-n-args state) 0))
             (set-up-call (fun call-n-args)
               (cond ((fn-p fun)
                      (setf (vm-state-fun state) fun
                            (vm-state-code state) (fn-code fun)
                            (vm-state-pc state) 0
                            (vm-state-env state) (fn-env fun)
                            (vm-state-n-args state) call-n-args))
                     ((pt-entity-p fun)
                      (let ((real-fun (%instance-proc fun)))
                        (unless (fn-p real-fun)
                          (machine-error "Entity procedure of ~A is not a function."
                                         fun))
			(setf (vm-state-fun state) real-fun
                              (vm-state-code state) (fn-code real-fun)
			      (vm-state-pc state) 0
			      (vm-state-env state) (fn-env real-fun)
			      (vm-state-n-args state) call-n-args)))
                     (t
                      (machine-error "Trying to call an unknown function: ~:W." fun))))
             (print-trace-information ()
               (when *trace-par-t-vm*
                 (format *trace-output* "~&Starting VM iteration:~%")
                 (format *trace-output* "  Function:~15T~A~%"
                         (fn-name (vm-state-fun state)))
                 (format *trace-output* "  Program Counter:~15T~D~%"
                         (vm-state-pc state))
                 (format *trace-output* "  Instruction:~15T~:W~%"
                         (vm-state-instr state))
                 (format *trace-output* "  Stack:~15T~:W~%"
                         (vm-state-stack state))
                 (unless (eq *trace-par-t-vm* :short)
                   (format *trace-output* "  Code:~15T~:W~%"
                           (vm-state-code state))
                   (format *trace-output* "  Environment:~15T~:W~%"
                           (vm-state-env state))
                   (format *trace-output* "  Number of Args:~15T~D~%"
                           (vm-state-n-args state))))))
      (loop
        (setf (vm-state-instr state)
              (elt (vm-state-code state) (vm-state-pc state)))
        (print-trace-information)
        (incf (vm-state-pc state))
        (case (opcode (vm-state-instr state))
          
          ;; Variable/stack manipulation instructions:
          (LVAR
           (let ((instr (vm-state-instr state))
                 (env (vm-state-env state)))
             (push (elt (elt env (arg1 instr)) (arg2 instr))
                   (vm-state-stack state))))
          (LSET
           (let ((instr (vm-state-instr state))
                 (env (vm-state-env state)))
             (setf (elt (elt env (arg1 instr)) (arg2 instr))
                   (top (vm-state-stack state)))))
          (GVAR 
           (multiple-value-bind (value presentp)
               (get-var-in-locale (arg1 (vm-state-instr state)) locale)
             (cond (presentp
                    (when *trace-par-t-global-loads*
                      (format *trace-output*
                              "~&>>> PT: accessing ~A~%" (arg1 (vm-state-instr state))))
                    (push value (vm-state-stack state)))
                   (t
                    (machine-error "Unbound global variable ~A.~%  ~
                                  Function: ~A:~%  ~
                                  Stack: ~:W"
                                   (arg1 (vm-state-instr state))
                                   (ignore-errors (fn-name (vm-state-fun state)))
                                   (vm-state-stack state))))))
          (GSET
           (setf (get-var-in-locale (arg1 (vm-state-instr state)) locale)
                 (top (vm-state-stack state))))
          (POP
           (pop (vm-state-stack state)))
          (CONST
           (push (arg1 (vm-state-instr state)) (vm-state-stack state)))
          
          ;; Branching instructions:
          (JUMP
           (setf (vm-state-pc state) (arg1 (vm-state-instr state))))
          (FJUMP
           (when (eq *false* (pop (vm-state-stack state)))
             (setf (vm-state-pc state) (arg1 (vm-state-instr state)))))
          (TJUMP
           (when (not (eq *false* (pop (vm-state-stack state))))
             (setf (vm-state-pc state) (arg1 (vm-state-instr state)))))
	  
          ;; Function call/return instructions:
          (SAVE
           (push (make-ret-addr :pc (arg1 (vm-state-instr state))
                                :fn (vm-state-fun state)
                                :env (vm-state-env state))
                 (vm-state-stack state)))
          (RETURN
            ;; The return value is the top of the stack; the return-address is
            ;; second if we don't return from the top level.
            (let* ((stack (vm-state-stack state))
                   (ret-addr (second stack)))
              (cond ((and ret-addr (ret-addr-p ret-addr))
                     (let ((fun (ret-addr-fn ret-addr)))
                       (setf (vm-state-fun state) fun
                             (vm-state-code state) (fn-code fun)
                             (vm-state-pc state) (ret-addr-pc ret-addr)
                             (vm-state-env state) (ret-addr-env ret-addr)))
                     ;; Get rid of the return-address, but keep the value on the
                     ;; stack.
                     (setf (vm-state-stack state) (cons (first stack) (rest2 stack))))
                    (t
                     ;; We are trying to return from the top level.  For
                     ;; simplicity we exit the machine in this case.
                     ;; This happens in particular when call/cc captures
                     ;; the outermost continuation, since there is no
                     ;; return address (and no value) on the stack.
                     ;; (warn "Returning from top level?")
                     (return-from machine (first stack))))))
          
          (CALLJ
           ;; Set the active function to the function object on the
           ;; stack.
           (let ((fun (pop (vm-state-stack state))))
             (set-up-call fun (arg1 (vm-state-instr state)))))
          (CALLJV
           ;; Set the active function to the function object on the stack and
           ;; the number of arguments to the second object on the stack.
           (let* ((fun (pop (vm-state-stack state)))
                  (n-args (pop (vm-state-stack state))))
             (set-up-call fun n-args)))
          
          (ARGS
           (let* ((instr (vm-state-instr state))
                  (arg (arg1 instr))
                  (n-args (vm-state-n-args state)))
             (unless (= n-args arg)
               (machine-error "Wrong number of arguments to function ~A: ~
                             ~D expected, ~D supplied"
                              (fn-name (vm-state-fun state))
                              arg
                              n-args))
             (push (make-array arg) (vm-state-env state))
             (loop for i from (- n-args 1) downto 0 do
                      (setf (elt (first (vm-state-env state)) i)
                            (pop (vm-state-stack state))))))
	  (VARARGS
	   (let* ((instr (vm-state-instr state))
                  (n-req (arg1 instr))
                  (n-args (vm-state-n-args state)))
             (unless (>= n-args n-req)
	       (machine-error "Wrong number of arguments to function ~A: ~
                               ~D or more expected, ~D supplied"
			      (fn-name (vm-state-fun state))
                              n-req
                              n-args))
	     (let ((args (make-array (+ 1 n-req) :initial-element '())))
	       (push args (vm-state-env state))
	       (loop repeat (- n-args n-req) do
                        (push (pop (vm-state-stack state)) (elt args n-req)))
	       (loop for i from (- n-req 1) downto 0 do
                        (setf (elt args i) (pop (vm-state-stack state)))))))
	  (FN
	   (push (make-fn :code (fn-code (arg1 (vm-state-instr state)))
			  :name (fn-name (arg1 (vm-state-instr state)))
			  :args (fn-args (arg1 (vm-state-instr state)))
			  :env (vm-state-env state)) 
		 (vm-state-stack state)))
          
	  (LISP-CALL
	   (let ((fun (fdefinition (pop (vm-state-stack state))))
		 (args (pop (vm-state-stack state))))
	     (push (apply fun args) (vm-state-stack state))))
	  (PRIM
	   (let ((fun (fdefinition (arg1 (vm-state-instr state))))
                 (n-args (vm-state-n-args state))
		 (args '()))
	     (dotimes (i n-args)
	       (push (pop (vm-state-stack state)) args))
	     (push (apply fun args) (vm-state-stack state))))
	  
	  ;; Continuation instructions:
	  (SET-CC
	   (setf (vm-state-stack state)
                 (top (vm-state-stack state))))
	  (CC
	   (push (make-fn
		  :env (list (vector (vm-state-stack state)))
		  :name '%%primitive-cc
		  :code '((ARGS 1)
                          (LVAR 1 0 ";"  state)
                          (SET-CC)
			  (LVAR 0 0)
                          (RETURN)))
		 (vm-state-stack state)))
	  
	  ;; Nullary operations:
	  #+(or)
	  ((PAR-T-READ NEWLINE) 
	   (push (funcall (opcode (vm-state-instr state))) (vm-state-stack state)))
	  
	  ;; Unary operations:
	  ((CAR CDR CADR PAR-T-NOT PAR-T-NULL
		PAR-T-CONSP PAR-T-BOOLEANP PAR-T-SYMBOLP PAR-T-FN-P 
		PAR-T-NUMBERP PAR-T-VECTORP PAR-T-CHARACTERP PAR-T-STRINGP
		%INSTANCE-CLASS %INSTANCE-PROC %INSTANCEP
		COMPILER) 
	   (push (funcall (opcode (vm-state-instr state))
                          (pop (vm-state-stack state)))
                 (vm-state-stack state)))
	  
	  ;; Binary operations:
	  ((+ - * / PAR-T-< PAR-T-> PAR-T-<= PAR-T->= PAR-T-/= PAR-T-=
	      CONS CAR-SETTER CDR-SETTER
	      %ALLOCATE-INSTANCE %ALLOCATE-ENTITY
	      %INSTANCE-REF
	      NAME! PAR-T-EQ PAR-T-EQUAL PAR-T-EQL)
           (let ((stack (vm-state-stack state))
                 (opcode (opcode (vm-state-instr state))))
             (setf (vm-state-stack state)
                   (cons (funcall opcode (second stack) (first stack))
                         (rest2 (vm-state-stack state))))))
	  
	  ;; Ternary operations:
	  ((%INSTANCE-SETTER %INSTANCE-PROC-SETTER)
           (let ((stack (vm-state-stack state))
                 (opcode (opcode (vm-state-instr state))))
             (setf (vm-state-stack state)
                   (cons (funcall opcode
                                  (third stack) (second stack) (first stack))
                         (rest3 (vm-state-stack state))))))
	  
	  ;; Constants:
	  ((PAR-T-TRUE)
	   (push *true* (vm-state-stack state)))

	  ((PAR-T-FALSE)
	   (push *false* (vm-state-stack state)))

	  ((-1 0 1 2)
	   (push (opcode (vm-state-instr state)) (vm-state-stack state)))
	  
	  ;; Other:
	  ((HALT)
	   (RETURN (top (vm-state-stack state))))

	  (otherwise (error "Unknown opcode: ~A"
                            (vm-state-instr state))))))))

