;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Based on code from Paradigms of Artificial Intelligence
;;; Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Bug fixes by Erann Gat, gat@aig.Jpl.Nasa.Gov, November 1992
;;; Modifications Copyright (c) 2012 Matthias Hölzl

;;; The modifications to this file are licensed under the MIT license;
;;; see the file LICENSE in the root directory for further
;;; information.

;;; Par-T compiler with assembler and peephole optimizer.

;;; TODO: There should be no Lisp errors or assertions in the real
;;; compiler; instead it should issue a warning and compile an error
;;; call instead.

(in-package :parallel-thetis)

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

#+(or)
(defun get-global-var (var)
  (let* ((default "unbound")
         (val (get var 'global-val default)))
    (if (eq val default)
        (error "Unbound par-t variable: ~a" var)
        val)))

;;; ==============================

(defun par-t-macro (symbol)
  (and (symbolp symbol) (get symbol 'par-t-macro)))

(defmacro define-par-t-macro (name parmlist &body body)
  "Define a Par-T macro."
  `(setf (get ',name 'par-t-macro)
         #'(lambda ,parmlist .,body)))

(defun par-t-macro-expand (x)
  "Macro-expand this Par-T expression."
  (if (and (listp x) (par-t-macro (first x)))
      (par-t-macro-expand
        (apply (par-t-macro (first x)) (rest x)))
      x))

(defun par-t-macro-expand-1 (x)
  "Macro-expand this Par-T expression once."
  (if (and (listp x) (par-t-macro (first x)))
      (apply (par-t-macro (first x)) (rest x))
      x))

;;; ==============================

;;; TODO: We need a way to define new setters as macros.
(defparameter *setters*
  '((car . car-setter!)
    (cdr . cdr-setter!)))

(define-par-t-macro set! (place value)
  (if (symbolp place)
      `(lset! ,place ,value)
      (let ((setter (cdr (assoc (first place) *setters*))))
	(if setter
	    ;; If a setter exists, generate a direct call.
	    `(,setter ,value ,@(rest place))
	    ;; Otherwise call the generic function `setter' to allow
	    ;; dispatch on the car.
	    `((setter ,(car place)) ,value ,@(rest place))))))

(define-par-t-macro let (bindings &rest body)
  (if (symbolp bindings)
      (let ((fun bindings)
            (bindings (first body))
            (body (rest body)))
        `(letrec ((,fun (lambda ,(mapcar #'first bindings)
                        ,@body)))
            (,fun ,@(mapcar #'second bindings))))
      `((lambda ,(mapcar #'first bindings) . ,body)
        ,@(mapcar #'second bindings))))

(define-par-t-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(define-par-t-macro and (&rest args)
  (cond ((null args) *true*)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(define-par-t-macro or (&rest args)
  (cond ((null args) *false*)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(define-par-t-macro cond (&rest clauses)
  (cond ((null clauses) *false*)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(define-par-t-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                        `((member ,key-val ',(first clause))
                          .,(rest clause))))
                clauses)))))

#+(or)
(define-par-t-macro define (name &rest body)
  (if (atom name)
      `(begin (lset! ,name . ,body) ',name)
      `(define ,(first name) 
         (lambda ,(rest name) . ,body))))

(define-par-t-macro define (name &rest body)
  (if (atom name)
      `(name! (lset! ,name . ,body) ',name)
      (par-t-macro-expand
         `(define ,(first name) 
            (lambda ,(rest name) . ,body)))))

(define-par-t-macro delay (computation)
  `(lambda () ,computation))

(define-par-t-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) *false*)) bindings)
     ,@(mapcar #'(lambda (v) `(lset! .,v)) bindings)
     ,@body))

;;; ==============================

(defvar *label-num* 0)

(defun compiler (x)
  "Compile an expression as if it were in a parameterless lambda."
  (setf *label-num* 0)
  (comp-lambda '() (list x) nil))

(defun comp-show (x)
  "Compile an expression and show the resulting code"
   (show-fn (compiler x))
  (values))

;;; ==============================

(defun gen (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun gen-label (&optional (label 'L))
  "Generate a label (a symbol of the form Lnnn)"
  (intern (format nil "~a~d" label (incf *label-num*))))

;;; ==============================

(defun gen-var (var env)
  "Generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LVAR (first p) (second p) ";" var)
        (gen 'GVAR var))))

;;; ==============================

(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)

;;; This should probably go into init-par-t-comp
(set-global-var! 'name! #'name!)

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (fn-name fn) '??)))

(defun label-p (x) "Is x a label?" (atom x))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (if (symbolp symbol)
      (let ((frame (find symbol env :test #'find)))
        (if frame
            (list (position frame env) (position symbol frame))
            nil))
      nil))

(defun comp (x env val? more?)
  "Compile the expression x into a list of instructions"
    (cond
      ((par-t-boolean-p x) (comp-const x val? more?))
      ((symbolp x) (comp-var x env val? more?))
      ((atom x) (comp-const x val? more?))
      ((par-t-macro (first x)) (comp (par-t-macro-expand x) env val? more?))
      ((case (first x)
         (QUOTE  (arg-count x 1)
                 (comp-const (second x) val? more?))
         (BEGIN  (comp-begin (rest x) env val? more?))
         (LSET!   (arg-count x 2)
                 (assert (symbolp (second x)) (x)
                         "Only symbols can be lset!, not ~a in ~a"
                         (second x) x)
                 (seq (comp (third x) env t t)
                      (gen-set (second x) env)
                      (if (not val?) (gen 'POP))
                      (unless more? (gen 'RETURN))))
         (IF     (arg-count x 2 3)
                 (comp-if (second x) (third x) (or (fourth x) *false*)
                          env val? more?))
         (LAMBDA (when val?
                   (let ((f (comp-lambda (second x) (rest2 x) env)))
                     (seq (gen 'FN f) (unless more? (gen 'RETURN))))))
         (t      (comp-funcall (first x) (rest x) env val? more?))))))

;;; ==============================

(defun arg-count (form min &optional (max min))
  "Report an error if form has wrong number of args."
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
      "Wrong number of arguments for ~a in ~a: 
       ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))

;;; ==============================

(defun comp-begin (exps env val? more?)
  "Compile a sequence of expressions,
  returning the last one as the value."
  (cond ((null exps) (comp-const *false* val? more?))
        ((length=1 exps) (comp (first exps) env val? more?))
        (t (seq (comp (first exps) env nil t)
                (comp-begin (rest exps) env val? more?)))))

(defun comp-list (exps env)
  "Compile a list, leaving them all on the stack."
  (if (null exps) nil
      (seq (comp (first exps) env t t)
           (comp-list (rest exps) env))))

;;; ==============================

(defun comp-const (x val? more?)
  "Compile a constant expression."
  (if val? (seq (cond ((par-t-true-p x)
		       (gen 'PAR-T-TRUE))
		      ((par-t-false-p x)
		       (gen 'PAR-T-FALSE))
		      ((member x '(-1 0 1 2))
		       (gen x))
		      (t
		       (gen 'CONST x)))
                (unless more? (gen 'RETURN)))))

(defun comp-var (x env val? more?)
  "Compile a variable reference."
  (if val? (seq (gen-var x env) (unless more? (gen 'RETURN)))))

;;; ==============================

(defun comp-if (pred then else env val? more?)
  "Compile a conditional (IF) expression."
  (cond
    ((par-t-false-p pred)    ; (if #f x y) ==> y
     (comp else env val? more?))
    ((par-t-true-p pred)     ; (if #t x y) ==> x
     (comp then env val? more?))
    ((and (listp pred)       ; (if (not p) x y) ==> (if p y x)
          (length=1 (rest pred))
          (primitive-p (first pred) env 1)
          (eq (prim-opcode (primitive-p (first pred) env 1)) 'not))
     (comp-if (second pred) else then env val? more?))
    (t (let ((pcode (comp pred env t t))
             (tcode (comp then env val? more?))
             (ecode (comp else env val? more?)))
         (cond
           ((equal tcode ecode) ; (if p x x) ==> (begin p x)
            (seq (comp pred env nil t) ecode))
           ((null tcode)  ; (if p nil y) ==> p (TJUMP L2) y L2:
            (let ((L2 (gen-label)))
              (seq pcode (gen 'TJUMP L2) ecode (list L2)
                   (unless more? (gen 'RETURN)))))
           ((null ecode)  ; (if p x) ==> p (FJUMP L1) x L1: 
            (let ((L1 (gen-label)))
              (seq pcode (gen 'FJUMP L1) tcode (list L1)
                   (unless more? (gen 'RETURN)))))
           (t             ; (if p x y) ==> p (FJUMP L1) x L1: y
                          ; or p (FJUMP L1) x (JUMP L2) L1: y L2:
            (let ((L1 (gen-label))
                  (L2 (if more? (gen-label))))
              (seq pcode (gen 'FJUMP L1) tcode
                   (if more? (gen 'JUMP L2))
                   (list L1) ecode (if more? (list L2))))))))))

;;; ==============================

(defun comp-funcall (f args env val? more?)
  "Compile an application of a function to arguments."
  (let ((prim (primitive-p f env (length args))))
    (cond
      (prim  ; function compilable to a primitive instruction
       (if (and (not val?) (not (prim-side-effects prim)))
           ;; Side-effect free primitive when value unused
           (comp-begin args env nil more?)
           ;; Primitive with value or call needed
           (seq (comp-list args env)
		(gen (prim-opcode prim))
		(unless val? (gen 'POP))
		(unless more? (gen 'RETURN)))))
      ((and (starts-with f 'lambda) (null (second f)))
       ;; ((lambda () body)) => (begin body)
       (assert (null args) () "Too many arguments supplied")
       (comp-begin (rest2 f) env val? more?))
      (more? ; Need to save the continuation point
       (let ((k (gen-label 'k)))
         (seq (gen 'SAVE k)
              (comp-list args env)
              (comp f env t t)
              (gen 'CALLJ (length args))
              (list k)
              (if (not val?) (gen 'POP)))))
      (t     ; function call as rename plus goto
       (seq (comp-list args env)
            (comp f env t t)
            (gen 'CALLJ (length args)))))))

;;; ==============================

;;; ==============================

(defun gen-set (var env)
  "Generate an instruction to set a variable to top-of-stack."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LSET (first p) (second p) ";" var)
        (if (assoc var *primitive-fns*)
            (error "Can't alter the constant ~a" var)
            (gen 'GSET var)))))

;;; ==============================

(defun comp-lambda (args body env)
  "Compile a lambda form into a closure with compiled code."
  (new-fn :env env :args args
	  :name `(lambda ,args ,@body)
          :code (seq (gen-args args 0)
                     (comp-begin body
                                 (cons (make-true-list args) env)
                                 t nil))))

(defun gen-args (args n-so-far)
  "Generate an instruction to load the arguments."
  (cond ((null args) (gen 'ARGS n-so-far))
        ((symbolp args) (gen 'VARARGS n-so-far))
        ((and (consp args) (symbolp (first args)))
         (gen-args (rest args) (+ n-so-far 1)))
        (t (error "Illegal argument list"))))

(defun make-true-list (dotted-list)
  "Convert a possibly dotted list into a true, non-dotted list."
  (cond ((null dotted-list) nil)
        ((atom dotted-list) (list dotted-list))
        (t (cons (first dotted-list)
                 (make-true-list (rest dotted-list))))))

(defun new-fn (&key code env name args)
  "Build a new function."
  (assemble (make-fn :env env :name name :args args
                     :code (optimize-bytecode code))))

;;; ==============================

(defun opcode (instr) (if (label-p instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defsetf arg1 (instr) (val) `(setf (second ,instr) ,val))

;;; ==============================

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (fn-code fn))
    (setf (fn-code fn)
          (asm-second-pass (fn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (when (is instr '(JUMP TJUMP FJUMP SAVE))
          ;; Don't modify instr, in case it is a quoted literal.
          (setf instr (copy-list instr))
          (setf (arg1 instr)
                (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ==============================

(defun show-fn (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (fn-p fn))
      (format stream "~8A" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (fn-code fn)))
          (let ((instr (elt (fn-code fn) i)))
            (if (label-p instr)
                (format stream "~A:" instr)
                (progn
                  (format stream "~VT~2D: " indent i)
                  (dolist (arg instr)
                    (show-fn arg stream (+ indent 8)))
                  (fresh-line))))))))

;;; ==============================

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op) 
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun init-par-t-comp ()
  "Initialize values (including call/cc) for the Par-T compiler."
  ;; Global constants
  (set-global-var! 'true *true*)
  (set-global-var! 'false *false*)

  ;; Applying functions
  (let ((%%apply (new-fn :name '%%apply :args '(proc length lst)
                         :code '((ARGS 3)
                                 LOOP
                                 (LVAR 0 2 ";" lst)
                                 (PAR-T-NULL)
                                 (TJUMP EXIT)
                                 (LVAR 0 1 ";" length)
                                 (1)
                                 (+)
                                 (LSET 0 1 ";" length)
                                 (POP)
                                 (LVAR 0 2 ";" lst)
                                 (CAR)
                                 (LVAR 0 2 ";" lst)
                                 (CDR)
                                 (LSET 0 2 ";" lst)
                                 (POP)
                                 (JUMP LOOP)
                                 EXIT
                                 (LVAR 0 1 ";" length)
                                 (LVAR 0 0 ";" proc)
                                 (CALLJV)))))
    (set-global-var! '%apply
      (new-fn :name '%apply :args '(proc lst)
              :env (list (vector %%apply))
              :code '((ARGS 2)
                      (LVAR 0 0 ";" proc)
                      (0)
                      (LVAR 0 1 ";" lst)
                      (LVAR 1 0 ";" %%apply)
                      (CALLJ 3)))))

  ;; Leaving the compiler
  (set-global-var! 'exit 
     (new-fn :name 'exit :args '(val) :code '((HALT))))

  ;; Support for the object system
  (let ((class-metaclass
          (%allocate-instance nil (length *the-slots-of-a-class*))))
    (setf (pt-object-class class-metaclass) class-metaclass)
    (set-global-var! '<class> class-metaclass))
  (set-global-var! '*the-slots-of-a-class* *the-slots-of-a-class*)

  ;; Continuation manipulation
  (set-global-var! 'call/cc
    (new-fn :name 'call/cc :args '(f)
            :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
		    (CALLJ 1))))

  ;; File handling
  (set-global-var! 'herald
    (new-fn :name 'herald :args '(title . args)
	    :code '((VARARGS 1)
		    (LVAR 0 0 ";" title)
		    (RETURN))))

  ;; Primitive functions
  (dolist (prim *primitive-fns*)
     (setf (get (prim-symbol prim) 'global-val)
           (new-fn :env nil :name (prim-symbol prim)
                   :code (seq (gen 'PRIM (prim-opcode prim))
                              (gen 'RETURN))))))

;;; ==============================

(defparameter *par-t-top-level*
  '(call/cc (lambda (cc)
	      (let ((%result '()))
		(define par-t (lambda ()
				 (newline)
				 (display "=> ")
				 (lset! %result ((compiler (read))))
				 (write %result)
				 (par-t)))
		(define quit (lambda ()
			       (cc %result)))
		(par-t)))))

(defun par-t ()
  "A compiled Par-T read-eval-print loop"
  (init-par-t-comp)
  (let ((par-t-code (compiler *par-t-top-level*)))
    (machine par-t-code par-t-code)))

(defun comp-go (exp)
  "Compile and execute the expression."
  (machine (compiler `(exit ,exp))))

;;;; Peephole Optimize-Bytecoder


;;; ==============================

(defun optimize-bytecode (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Optimize each tail  
    (loop for code-tail on code do
          (setf any-change (or (optimize-1 code-tail code)
                               any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize-bytecode code)
        code)))

;;; ==============================

(defun optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizer (get-optimizer (opcode instr))))
    (when optimizer
      (funcall optimizer instr code all-code))))

;;; ==============================

(let ((optimizers (make-hash-table :test #'eql)))

  (defun get-optimizer (opcode)
    "Get the assembly language optimizer for this opcode."
    (gethash opcode optimizers))

  (defun put-optimizer (opcode fn)
    "Store an assembly language optimizer for this opcode."
    (setf (gethash opcode optimizers) fn)))

;;; ==============================

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

;;; ==============================

(defmacro def-optimizer (opcodes args &body body)
  "Define assembly language optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))

;;;; Now for some additions and answers to exercises:

;;; ==============================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'eof)
    (defconstant eof "EoF")))
(defun eof-object? (x) (eq x eof))
(defvar *par-t-readtable* (copy-readtable))
;; To restore the original readtable once we have set it to *PAR-T-READTABLE*
;; (for testing purposes).  Copy the readtable in case we accidentially modify
;; it.
(defvar *cl-readtable* (copy-readtable))

(defun par-t-read (&optional (stream *standard-input*))
  (let ((*readtable* *par-t-readtable*))
    (read stream nil eof)))

;;; ==============================

(set-dispatch-macro-character #\# #\t 
  #'(lambda (&rest ignore)
      (declare (ignore ignore))
      *true*)
  *par-t-readtable*)

(set-dispatch-macro-character #\# #\f 
  #'(lambda (&rest ignore)
      (declare (ignore ignore))
      *false*)
  *par-t-readtable*)

(set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Par-T,
  ;; #x, #o and #b are hexidecimal, octal, and binary,
  ;; e.g. #xff = #o377 = #b11111111 = 255
  ;; In Par-T only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore) 
      (declare (ignore ignore))
      (let ((*read-base* 10)) (par-t-read stream)))
  *par-t-readtable*)

(set-macro-character #\` 
  #'(lambda (s ignore)
      (declare (ignore ignore))
      (list 'quasiquote (par-t-read s))) 
  nil *par-t-readtable*)

(set-macro-character #\, 
   #'(lambda (stream ignore)
       (declare (ignore ignore))
       (let ((ch (read-char stream)))
         (if (char= ch #\@)
             (list 'unquote-splicing (read stream))
             (progn (unread-char ch stream)
                    (list 'unquote (read stream))))))
   nil *par-t-readtable*)

;;; ==============================

;(setf (par-t-macro 'quasiquote) 'quasi-q)

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)      
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

;;; ==============================

(defvar *trace-par-t-reader* nil)

(defun load-par-t-file (file-name)
  (let ((result '()))
    (with-open-file (stream file-name :direction :input)
      (do ((form (par-t-read stream) (par-t-read stream)))
	  ((eof-object? form) (nreverse result))
        (when *trace-par-t-reader*
          (format *trace-output*
                  "~&Reading: ~:W~%" form)
          (force-output *trace-output*))
        (let ((evaluation-result (comp-go form)))
          (when (or (stringp evaluation-result) (symbolp evaluation-result))
            (push evaluation-result result)))))))

(defun par-t-system-file (name)
  (merge-pathnames
   (make-pathname :name name :type "pt")
   (directory-namestring (asdf:system-source-file :par-t))))

(defun load-par-t-standard-library ()
  (let ((stdlib (par-t-system-file "standard-library"))
        (object-system (par-t-system-file "objects")))
    (flet ((print-herald (list)
             (format t "Loading ~A~%" (first list))
             (format t "Defined ~:W~%" (rest list))
             (force-output)))
      (print-herald (load-par-t-file stdlib))
      (print-herald (load-par-t-file object-system)))))

(defun load-par-t-compiler ()
  (let ((filename (par-t-system-file "compiler")))
    (load-par-t-file filename)))

(defun load-par-t-tests ()
  (init-par-t-comp)
  (load-par-t-standard-library)
  (let ((filename (par-t-system-file "tests")))
    (load-par-t-file filename)))


;;; Set-up the compiler.
;;; ===================

;;; We call `init-par-t-com' after loading this file so that we don't
;;; have to remember calling it before we do something with the
;;; compiler in the toplevel.

(init-par-t-comp)
