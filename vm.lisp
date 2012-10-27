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

(defun machine (f &optional (error-fun *error-fun*))
  "Run the abstract machine on the code for f."
  (declare (optimize (debug 3)))
  (assert (fn-p error-fun) () "The error function has to be a function object.")
  (let* ((code (fn-code f))
         (pc 0)
         (env nil)
         (stack nil)
         (n-args 0)
         (instr nil))
    (labels ((machine-error (format-string &rest args)
	       (apply #'cerror 
		      "Restart the computation with the error function."
		      format-string args)
	       (setf f error-fun
		     code (fn-code f)
		     pc 0
		     env nil
		     n-args 0))
	     (set-up-call (fun call-n-args)
	       (cond ((fn-p fun)
		      (setf f fun)
		      (setf code (fn-code fun)
			    env (fn-env fun)
			    pc 0
			    n-args call-n-args))
		     ((pt-entity-p fun)
		      (let ((real-fun (%instance-proc fun)))
			(setf f real-fun)
			(unless (fn-p real-fun)
			  (machine-error "Entity procedure of ~A is not a function."
					 fun))
			(setf code (fn-code real-fun)
			      env (fn-env real-fun)
			      pc 0
			      n-args call-n-args)))
		     (t
		      (machine-error "Trying to call an unknown function: ~:W." fun))))
	     (print-trace-information ()
	       (when *trace-par-t-vm*
		 (format *trace-output* "~&Starting VM iteration:~%")
		 (format *trace-output* "  Function:~15T~A~%" (fn-name f))
		 (format *trace-output* "  Program Counter:~15T~D~%" pc)
		 (format *trace-output* "  Instruction:~15T~:W~%" instr)
		 (format *trace-output* "  Stack:~15T~:W~%" stack)
		 (unless (eq *trace-par-t-vm* :short)
		   (format *trace-output* "  Code:~15T~:W~%" code)
		   (format *trace-output* "  Environment:~15T~:W~%" env)
		   (format *trace-output* "  Number of Args:~15T~D~%" n-args)))))
      (loop
	(setf instr (elt code pc))
	(print-trace-information)
	(incf pc)
	(case (opcode instr)
	  
	  ;; Variable/stack manipulation instructions:
	  (LVAR
	   (push (elt (elt env (arg1 instr)) (arg2 instr))
		 stack))
	  (LSET
	   (setf (elt (elt env (arg1 instr)) (arg2 instr))
		 (top stack)))
	  (GVAR 
	   (multiple-value-bind (indicator value)
	       (get-properties (symbol-plist (arg1 instr)) '(global-val))
	     (cond ((eq indicator 'global-val)
		    (when *trace-par-t-global-loads*
		      (format *trace-output*
			      "~&>>> PT: accessing ~A~%" (arg1 instr)))
		    (push value stack))
		   (t
		    (machine-error "Unbound global variable ~A.~%  ~
                                    Function: ~A:~%  ~
                                    Stack: ~:W"
				   (arg1 instr)
				   (ignore-errors (fn-name f))
				   stack)))))
	  (GSET
	   (setf (get (arg1 instr) 'global-val) (top stack)))
	  (POP
	   (pop stack))
	  (CONST
	   (push (arg1 instr) stack))
	  
	  ;; Branching instructions:
	  (JUMP
	   (setf pc (arg1 instr)))
	  (FJUMP
	   (when (eq *false* (pop stack))
	     (setf pc (arg1 instr))))
	  (TJUMP
	   (when (not (eq *false* (pop stack)))
	     (setf pc (arg1 instr))))
	  
	  ;; Function call/return instructions:
	  (SAVE
	   (push (make-ret-addr :pc (arg1 instr)
				:fn f :env env)
		 stack))
	  (RETURN
	    ;; The return value is the top of the stack; the
	    ;; return-address is second if we don't return from the
	    ;; top level.
	    (cond ((and (second stack) (ret-addr-p (second stack)))
		   (setf f (ret-addr-fn (second stack))
			 code (fn-code f)
			 env (ret-addr-env (second stack))
			 pc (ret-addr-pc (second stack)))
		   ;; Get rid of the return-address, but keep the
		   ;; value on the stack.
		   (setf stack (cons (first stack) (rest2 stack))))
		  (t
		   ;; We are trying to return from the top level.  For
		   ;; simplicity we exit the machine in this case.
		   ;; This happens in particular when call/cc captures
		   ;; the outermost continuation, since there is no
		   ;; return address (and no value) on the stack.
		   ;; (warn "Returning from top level?")
		   (return-from machine (first stack)))))
	  (CALLJ
	   ;; Set the active function to the function object on the
	   ;; stack.
	   (let ((fun (pop stack)))
	     (set-up-call fun (arg1 instr))))
	  (CALLJV
	   ;; Set the active function to the function object on the
	   ;; stack and the number of arguments to the second object
	   ;; on the stack.
	   (let* ((fun (pop stack))
		  (call-n-args (pop stack)))
	     (set-up-call fun call-n-args)))
	  (ARGS
	   (unless (= n-args (arg1 instr))
	     (machine-error "Wrong number of arguments to function ~A: ~
                             ~D expected, ~D supplied"
			    (fn-name f) (arg1 instr) n-args))
	   (push (make-array (arg1 instr)) env)
	   (loop for i from (- n-args 1) downto 0 do
	     (setf (elt (first env) i) (pop stack))))
	  (VARARGS
	   (let ((n-req (arg1 instr)))
	     (unless (>= n-args n-req)
	       (machine-error "Wrong number of arguments to function ~A: ~
                               ~D or more expected, ~D supplied"
			      (fn-name f) n-req n-args))
	     (let ((args (make-array (+ 1 n-req) :initial-element '())))
	       (push args env)
	       (loop repeat (- n-args n-req) do
		 (push (pop stack) (elt args n-req)))
	       (loop for i from (- n-req 1) downto 0 do
		 (setf (elt args i) (pop stack))))))
	  (FN
	   (push (make-fn :code (fn-code (arg1 instr))
			  :name (fn-name (arg1 instr))
			  :args (fn-args (arg1 instr))
			  :env env) 
		 stack))
	  (PRIM
	   (let ((fun (fdefinition (arg1 instr)))
		 (args '()))
	     (dotimes (i n-args)
	       (push (pop stack) args))
	   (push (apply fun args)
		 stack)))
	  
	  ;; Continuation instructions:
	  (SET-CC
	   (setf stack (top stack)))
	  (CC
	   (push (make-fn
		  :env (list (vector stack))
		  :name '%%primitive-cc
		  :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
			  (LVAR 0 0) (RETURN)))
		 stack))
	  
	  ;; Nullary operations:
	  ((PAR-T-READ NEWLINE) 
	   (push (funcall (opcode instr)) stack))
	  
	  ;; Unary operations:
	  ((CAR CDR CADR PAR-T-NOT PAR-T-NULL
		PAR-T-CONSP PAR-T-BOOLEANP PAR-T-SYMBOLP PAR-T-FN-P 
		PAR-T-NUMBERP PAR-T-VECTORP PAR-T-CHARACTERP PAR-T-STRINGP
		;; TODO: Streams or ports.
		%INSTANCE-CLASS %INSTANCE-PROC %INSTANCEP
		DISPLAY PAR-T-WRITE
		;; These three should not really be opcodes.
		COMPILER
		RANDOM GENSYM) 
	   (push (funcall (opcode instr) (pop stack)) stack))
	  
	  ;; Binary operations:
	  ((+ - * / PAR-T-< PAR-T-> PAR-T-<= PAR-T->= PAR-T-/= PAR-T-=
	      CONS CAR-SETTER CDR-SETTER
	      %ALLOCATE-INSTANCE %ALLOCATE-ENTITY
	      %INSTANCE-REF
	      NAME! PAR-T-EQ PAR-T-EQUAL PAR-T-EQL)
	   (setf stack (cons (funcall (opcode instr) (second stack)
				      (first stack))
			     (rest2 stack))))
	  
	  ;; Ternary operations:
	  ((%INSTANCE-SETTER %INSTANCE-PROC-SETTER)
	   (setf stack (cons (funcall (opcode instr) (third stack)
				      (second stack) (first stack))
			     (rest3 stack))))
	  
	  ;; Constants:
	  ((PAR-T-TRUE)
	   (push *true* stack))

	  ((PAR-T-FALSE)
	   (push *false* stack))

	  ((-1 0 1 2)
	   (push (opcode instr) stack))
	  
	  ;; Other:
	  ((HALT)
	   (RETURN (top stack)))

	  (otherwise (error "Unknown opcode: ~a" instr)))))))

