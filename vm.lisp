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
    (flet ((machine-error (format-string &rest args)
	     (apply #'cerror 
		    "Restart the computation with the error function."
		    format-string args)
	     (setf f error-fun
		   code (fn-code f)
		   pc 0
		   env nil
		   n-args 0)))
      (loop
	(setf instr (elt code pc))
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
		    (push value stack))
		   (t
		    (machine-error "Unbound global variable ~A."
				   (arg1 instr))))))
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
	   (if (null (pop stack)) (setf pc (arg1 instr))))
	  (TJUMP
	   (if (pop stack) (setf pc (arg1 instr))))
	  
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
	   ;; First, discard the environment of the currently active
	   ;; function.  Is this really necessary?  In the success
	   ;; case we overwrite the value of `env' anyway with the
	   ;; function environment. --tc
	   (pop env)
	   ;; Set the active function to the function object on the
	   ;; stack.
	   (setf f (pop stack))
	   (cond ((fn-p f)
		  (setf code (fn-code f)
			env (fn-env f)
			pc 0
			n-args (arg1 instr)))
		 ;; Tried to call something other than a function
		 ;; structure.  This is the place where support for
		 ;; callable instances (aka entities) should go.
		 (t
		  (machine-error "Trying to call an unknown function."))))
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
	  ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM) 
	   (push (funcall (opcode instr) (pop stack)) stack))
	  
	  ;; Binary operations:
	  ((+ - * / < > <= >= /= = CONS LIST2 SETF-CAR SETF-CDR NAME! EQ EQUAL EQL)
	   (setf stack (cons (funcall (opcode instr) (second stack)
				      (first stack))
			     (rest2 stack))))
	  
	  ;; Ternary operations:
	  (LIST3
	   (setf stack (cons (funcall (opcode instr) (third stack)
				      (second stack) (first stack))
			     (rest3 stack))))
	  
	  ;; Constants:
	  ((T NIL -1 0 1 2)
	   (push (opcode instr) stack))
	  
	  ;; Other:
	  ((HALT) (RETURN (top stack)))
	  (otherwise (error "Unknown opcode: ~a" instr)))))))

