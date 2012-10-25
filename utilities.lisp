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

;;; The definition of a function for the VM.
;;; ---------------------------------------

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

;;; Primitives known by the VM.
;;; --------------------------

(defstruct (prim (:type list)) 
  symbol n-args opcode always side-effects)

(defun car-setter (new-value cons)
  (setf (car cons) new-value))

(defun cdr-setter (new-value cons)
  (setf (cdr cons) new-value))

(defun booleanp (obj)
  (or (eq obj nil)
      (eq obj t)))

(defstruct pt-object
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

(defun %instance-proc (obj)
  (pt-entity-proc obj))

(defun %instance-proc-setter (new-proc obj)
  (setf (pt-entity-proc obj) new-proc))

(defun %instancep (obj)
  (pt-object-p obj))

(defun %instance-ref (obj index)
  (svref (pt-object-slots obj) index))

(defun %instance-setter (new-value obj index)
  (setf (svref (pt-object-slots obj) index) new-value))

(defparameter *primitive-fns*
  '(;; Arithmetic and numerical comparisons
    (+ 2 + true nil)
    (- 2 - true nil)
    (* 2 * true nil)
    (/ 2 / true nil)
    (< 2 < nil nil)
    (> 2 > nil nil)
    (<= 2 <= nil nil)
    (>= 2 >= nil nil)
    (/= 2 /= nil nil)
    (= 2 = nil nil)

    ;; General comparisons and logical functions
    (eq? 2 eq nil nil)
    (eqv? 2 eql nil nil)
    (equal? 2 equal nil nil)
    (not 1 not nil nil)

    ;; Type predicates
    (null? 1 not nil nil)
    (pair? 1 consp nil nil)
    (boolean? 1 booleanp nil nil)
    (symbol? 1 symbolp nil nil)
    ;; Maybe this should include entities as well?
    (procedure? 1 fn-p nil nil)
    (number? 1 numberp nil nil)
    (vector? 1 vectorp nil nil)
    (char? 1 characterp nil nil)
    (string? 1 stringp nil nil)
    
    ;; Primitives for lists
    (cons 2 cons true nil)
    (car 1 car nil nil)
    (car-setter! 2 car-setter nil nil)
    (cdr 1 cdr nil nil)
    (cdr-setter! 2 cdr-setter nil nil)
    (cadr 1 cadr nil nil) 
    (list 1 list1 true nil)
    (list 2 list2 true nil)
    (list 3 list3 true nil)

    ;; Instance handling for the object system
    (%allocate-instance 2 %allocate-instance true nil)
    (%allocate-entity 2 %allocate-entity true nil)
    (%instance-class 1 %instance-class nil nil)
    (%instance-proc 1 %instance-proc nil nil)
    (%instance? 1 %instancep nil nil)
    (%instance-ref 2 %instance-ref nil nil)
    (%instance-setter 3 %instance-setter nil t)
    (%instance-proc-setter 3 %instance-proc-setter nil t)

    ;; Reading and writing
    (read 0 par-t-read nil t)
    (eof-object? 1 eof-object? nil nil)
    (write 1 write nil t)
    (display 1 display nil t)
    (newline 0 newline nil t)

    ;; Misc stuff
    (compiler 1 compiler t nil) 
    (name! 2 name! true t)
    (random 1 random true nil)))
  

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))
(defun display (x) (princ x))
(defun newline () (terpri))

