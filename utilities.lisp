;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:parallel-thetis)

;;; Some utility functions from PAIP.
;;; ================================

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns t if exps is nil, exps
  if there is only one, and (and exp1 exp2...) if there are several
  exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))


;;; Structures used in both the VM and the compiler.
;;; ===============================================

;;; The Boolean values.
;;; ------------------

(defstruct par-t-boolean)

(defstruct (par-t-true
	    (:include par-t-boolean)
	    (:print-function (lambda (obj stream depth)
			       (declare (ignore obj depth))
			       (format stream "#t")))))
(defstruct (par-t-false
	    (:include par-t-boolean)
	    (:print-function (lambda (obj stream depth)
			       (declare (ignore obj depth))
			       (format stream "#f")))))

(defvar *true* (make-par-t-true))
(defvar *false* (make-par-t-false))

(defmethod make-load-form ((self par-t-true) &optional env)
  (declare (ignore self env))
  '*true*)

(defmethod make-load-form ((self par-t-false) &optional env)
  (declare (ignore self env))
  '*false*)

;;; The definition of a function for the VM.
;;; ---------------------------------------

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (print-unreadable-object (fn stream :type t)
    (format stream "~A" (or (fn-name fn) '??))))

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

;;; Locales
;;; -------

(defun print-locale (locale &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (print-unreadable-object (locale stream :type t :identity t)
    (format stream "~A" (locale-identifier locale))))

(defstruct (locale (:print-function print-locale))
  identifier
  (bindings (make-hash-table))
  (superior-locale nil))

(defvar *top-level-locale* (make-locale :identifier :top-level-locale))

(defun top-level-locale ()
  *top-level-locale*)

(defun (setf top-level-locale) (new-locale)
  (setf *top-level-locale* new-locale))

(defun get-var-in-locale (var &optional (locale *top-level-locale*) (default nil))
  (multiple-value-bind (value presentp)
      (gethash var (locale-bindings locale) default)
    (cond (presentp
           (values value presentp))
          ((locale-superior-locale locale)
           (get-var-in-locale var (locale-superior-locale locale) default))
          (t
           (values default nil)))))

(defun (setf get-var-in-locale) (new-value var &optional (locale *top-level-locale*))
  (setf (gethash var (locale-bindings locale)) new-value))

;;; Primitives known by the VM.
;;; --------------------------

(defstruct (prim (:type list)) 
  symbol n-args opcode always side-effects)

(defun par-t-not (obj)
  (if (par-t-false-p obj)
      *true*
      *false*))

(declaim (inline as-par-t-boolean))
(defun as-par-t-boolean (value)
  (if value
      *true*
      *false*))

(defun par-t-< (lhs rhs)
  (as-par-t-boolean (< lhs rhs)))

(defun par-t-> (lhs rhs)
  (as-par-t-boolean (> lhs rhs)))

(defun par-t-<= (lhs rhs)
  (as-par-t-boolean (<= lhs rhs)))

(defun par-t->= (lhs rhs)
  (as-par-t-boolean (>= lhs rhs)))

(defun par-t-/= (lhs rhs)
  (as-par-t-boolean (/= lhs rhs)))

(defun par-t-= (lhs rhs)
  (as-par-t-boolean (= lhs rhs)))

(defun par-t-eq (lhs rhs)
  (as-par-t-boolean (eq lhs rhs)))

(defun par-t-eql (lhs rhs)
  (as-par-t-boolean (eql lhs rhs)))

(defun par-t-equal (lhs rhs)
  (as-par-t-boolean (equal lhs rhs)))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))

(defun car-setter (new-value cons)
  (setf (car cons) new-value))

(defun cdr-setter (new-value cons)
  (setf (cdr cons) new-value))

(defun par-t-null (obj)
  (as-par-t-boolean (null obj)))

(defun par-t-consp (obj)
  (as-par-t-boolean (consp obj)))

(defun par-t-booleanp (obj)
  (as-par-t-boolean (par-t-boolean-p obj)))

(defun par-t-symbolp (obj)
  (as-par-t-boolean (symbolp obj)))

(defun par-t-fn-p (obj)
  (as-par-t-boolean (fn-p obj)))

(defun par-t-numberp (obj)
  (as-par-t-boolean (numberp obj)))

(defun par-t-vectorp (obj)
  (as-par-t-boolean (vectorp obj)))

(defun par-t-characterp (obj)
  (as-par-t-boolean (characterp obj)))

(defun par-t-stringp (obj)
  (as-par-t-boolean (stringp obj)))

(defstruct (pt-object
	    (:print-function
	     (lambda (obj stream depth)
	       (declare (ignore depth))
	       (print-unreadable-object (obj stream :type t :identity t)
		 (let ((slots (pt-object-slots obj)))
		   (when (and slots (> (length slots) 0))
		     (let ((name (elt slots 0)))
		       (when (symbolp name)
			 (format stream "~A" name)))))))))
  class
  slots)

(defun %allocate-instance (class n-slots)
  (make-pt-object :class class
		  :slots (make-array n-slots)))

(defstruct (pt-entity (:include pt-object))
  proc)

(defun %allocate-entity (class n-slots)
  (make-pt-entity :class class
		  :slots (make-array n-slots)))

(defun %instance-class (obj)
  (pt-object-class obj))

(defun %instance-class-setter (new-class obj)
  (setf (pt-object-class obj) new-class))

(defun %instance-proc (obj)
  (pt-entity-proc obj))

(defun %instance-proc-setter (new-proc obj)
  (setf (pt-entity-proc obj) new-proc))

(defun %instancep (obj)
  (as-par-t-boolean (pt-object-p obj)))

(defun %instance-ref (obj index)
  (svref (pt-object-slots obj) index))

(defun %instance-setter (new-value obj index)
  (setf (svref (pt-object-slots obj) index) new-value))


;;; Accessing a function's code
;;; ===========================

(defun fun-code (funlike)
  (typecase funlike
    (fn (fn-code funlike))
    (pt-entity (fn-code (pt-entity-proc funlike)))))

;;; Print functions
;;; ===============

(defun display (x)
  (cond ((null x)
	 (format t "()"))
	(t
	 (princ x)))
  x)

(defun par-t-write (x)
  (cond ((null x)
	 (format t "()"))
	(t
	 (write x)))
  x)

(defun newline ()
  (terpri)
  *false*)


(defparameter *primitive-fns*
  '(;; Arithmetic and numerical comparisons
    (+ 2 + true nil)
    (- 2 - true nil)
    (* 2 * true nil)
    (/ 2 / true nil)
    (< 2 par-t-< nil nil)
    (> 2 par-t-> nil nil)
    (<= 2 par-t-<= nil nil)
    (>= 2 par-t->= nil nil)
    (/= 2 par-t-/= nil nil)
    (= 2 par-t-= nil nil)

    ;; General comparisons and logical functions
    (eq? 2 par-t-eq nil nil)
    (eqv? 2 par-t-eql nil nil)
    (equal? 2 par-t-equal nil nil)
    (not 1 par-t-not nil nil)

    ;; Type predicates
    (null? 1 par-t-null nil nil)
    (pair? 1 par-t-consp nil nil)
    (boolean? 1 par-t-booleanp nil nil)
    (symbol? 1 par-t-symbolp nil nil)
    ;; Maybe this should include entities as well?
    (procedure? 1 par-t-fn-p nil nil)
    (number? 1 par-t-numberp nil nil)
    (vector? 1 par-t-vectorp nil nil)
    (char? 1 par-t-characterp nil nil)
    (string? 1 par-t-stringp nil nil)
    
    ;; Primitives for lists
    (cons 2 cons true nil)
    (car 1 car nil nil)
    (car-setter! 2 car-setter nil nil)
    (cdr 1 cdr nil nil)
    (cdr-setter! 2 cdr-setter nil nil)
    (cadr 1 cadr nil nil) 

    ;; Threads
    #+(or)
    (thread 0 current-thread nil nil)
    (spawn 3 spawn-thread true t)

    ;; Instance handling for the object system
    (%allocate-instance 2 %allocate-instance true nil)
    (%allocate-entity 2 %allocate-entity true nil)
    (%instance-class 1 %instance-class nil nil)
    (%instance-proc 1 %instance-proc nil nil)
    (%instance? 1 %instancep nil nil)
    (%instance-ref 2 %instance-ref nil nil)
    (%instance-setter! 3 %instance-setter nil t)
    (%instance-class-setter! 3 %instance-class-setter nil t)
    (%instance-proc-setter! 3 %instance-proc-setter nil t)

    ;; Misc stuff
    (compiler 1 compiler t nil) 
    (set-name! 2 set-name! true t)))
  

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

;;; Utilities for bootstrapping the object system

(defparameter *the-slots-of-a-class*
  '(name                       ; symbol (if known)
    direct-supers              ; (class ...)        
    direct-slots               ; ((name . options) ...)
    cpl                        ; (class ...) 
    slots                      ; ((name . options) ...) 
    n-fields                   ; an integer
    field-initializers         ; (proc ...)
    getters-and-setters))        ; ((slot-name getter setter) ...)
