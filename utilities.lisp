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

(defun setf-car (cons new-value)
  (setf (car cons) new-value))

(defun setf-cdr (cons new-value)
  (setf (cdr cons) new-value))

(defparameter *primitive-fns*
  '((+ 2 + true nil)
    (- 2 - true nil)
    (* 2 * true nil)
    (/ 2 / true nil)
    (< 2 < nil nil)
    (> 2 > nil nil)
    (<= 2 <= nil nil)
    (>= 2 >= nil nil)
    (/= 2 /= nil nil)
    (= 2 = nil nil)
    (eq? 2 eq nil nil)
    (eqv? 2 eql nil nil)
    (equal? 2 equal nil nil)
    (not 1 not nil nil)
    (null? 1 not nil nil)
    (cons 2 cons true nil)
    (car 1 car nil nil)
    (set-car! 2 setf-car nil nil)
    (cdr 1 cdr nil nil)
    (set-cdr! 2 setf-cdr nil nil)
    (cadr 1 cadr nil nil) 
    (list 1 list1 true nil)
    (list 2 list2 true nil)
    (list 3 list3 true nil)
    (read 0 par-t-read nil t)
    (eof-object? 1 eof-object? nil nil)
    (write 1 write nil t)
    (display 1 display nil t)
    (newline 0 newline nil t)
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

