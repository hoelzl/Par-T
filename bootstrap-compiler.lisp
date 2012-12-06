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

(defun set-global-var! (var val &optional (locale (top-level-locale)))
  (setf (get-var-in-locale var locale)
        val))

;;; ==============================

(defparameter *par-t-macros* '())

(defun make-par-t-call-1 (fun arg)
  (new-fn
   :code `((ARGS 0)
           (SAVE L1)
           (CONST ,arg)
           (GVAR ,fun)
           (CALLJ 1)
           L1
           (GVAR EXIT)
           (CALLJ 1))))

(defun par-t-macro-p (symbol)
  (not (eq *false*
           (run (make-par-t-call-1 'par-t-macro symbol)))))

(defun par-t-macro-expand (exp)
  "Macro-expand this Par-T expression."
  (let ((result (run (make-par-t-call-1 'macro-expand exp))))
    result))

(defun par-t-macro-expand-1 (exp)
  "Macro-expand this Par-T expression once."
  (run (make-par-t-call-1 'macro-expand-1 exp)))

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

(defun set-name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)

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
  (declare (optimize debug))
    (cond
      ((par-t-boolean-p x) (comp-const x val? more?))
      ((null x) (comp-const x val? more?))
      ((keywordp x) (comp-const x val? more?))
      ((symbolp x) (comp-var x env val? more?))
      ((atom x) (comp-const x val? more?))
      ((par-t-macro-p (first x)) (comp (par-t-macro-expand x) env val? more?))
      ((case (first x)
         (QUOTE  (arg-count x 1)
                 (comp-const (second x) val? more?))
         (BEGIN  (comp-begin (rest x) env val? more?))
         (LSET!   (arg-count x 2)
                 (cl:assert (symbolp (second x)) (x)
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
    (cl:assert (<= min n-args max) (form)
               "Wrong number of arguments for ~a in ~a: 
                ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))

;;; ==============================

(defun comp-begin (exps env val? more?)
  "Compile a sequence of expressions, returning the last one as the value."
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
       (cl:assert (null args) () "Too many arguments supplied")
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

(defun initialize-top-level-locale (&optional (locale (top-level-locale)))
  "Initialize values (including call/cc) for the Par-T compiler."
  ;; Global constants
  (set-global-var! 'true *true* locale)
  (set-global-var! 'false *false* locale)
  (set-global-var! 'set-name! #'set-name! locale)

  ;; Temporary definition needed for bootstrapping the compiler.
  (set-global-var! 'par-t-macro
    (new-fn :name 'par-t-macro :args '(symbol)
            :code '((ARGS 1)
                    (PAR-T-FALSE)
                    (RETURN)))
    locale)

  ;;; Getting the current thread
  (set-global-var! 'current-thread
    (new-fn :name 'current-thread :args '()
            :code '((ARGS 0)
                    (THREAD)
                    (RETURN)))
    locale)

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
                                 (LVAR 0 0 ";" proc)
                                 (LVAR 0 1 ";" length)
                                 (CALLJ-NARGS)))))
    (set-global-var! '%apply
      (new-fn :name '%apply :args '(proc lst)
              :env (list (vector %%apply))
              :code '((ARGS 2)
                      (LVAR 0 0 ";" proc)
                      (0)
                      (LVAR 0 1 ";" lst)
                      (LVAR 1 0 ";" %%apply)
                      (CALLJ 3)))
      locale))

  ;; Leaving the compiler
  (set-global-var! 'exit 
     (new-fn :name 'exit :args '(val) :code '((HALT)))
     locale)

  ;; Support for the object system
  (let ((class-metaclass
          (%allocate-instance nil (length *the-slots-of-a-class*))))
    (setf (pt-object-class class-metaclass) class-metaclass)
    (set-global-var! '<class> class-metaclass))
  (set-global-var! '*the-slots-of-a-class* *the-slots-of-a-class*
                   locale)

  ;; Continuation manipulation
  (set-global-var! 'call/cc
    (new-fn :name 'call/cc :args '(f)
            :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
		    (CALLJ 1)))
    locale)

  ;; File handling
  (set-global-var! 'herald
    (new-fn :name 'herald :args '(title . args)
	    :code '((VARARGS 1)
		    (LVAR 0 0 ";" title)
		    (RETURN)))
    locale)

  ;; Lisp functions
  (set-global-var! 'lisp-call-0
    (new-fn :name 'lisp-call-0 :args '(function-name)
            :code '((ARGS 1)
                    (LVAR 0 0 ";" function-name)
                    (LISP-CALL-0)
                    (RETURN))))

  (set-global-var! 'lisp-call-1
    (new-fn :name 'lisp-call-1 :args '(function-name arg1)
            :code '((ARGS 2)
                    (LVAR 0 0 ";" function-name)
                    (LVAR 0 1 ";" arg1)
                    (LISP-CALL-1)
                    (RETURN))))

  (set-global-var! 'lisp-call-2
    (new-fn :name 'lisp-call-2 :args '(function-name arg1 arg2)
            :code '((ARGS 3)
                    (LVAR 0 0 ";" function-name)
                    (LVAR 0 1 ";" arg1)
                    (LVAR 0 2 ";" arg2)
                    (LISP-CALL-2)
                    (RETURN))))

  (set-global-var! 'lisp-call-3
    (new-fn :name 'lisp-call-3 :args '(function-name arg1 arg2 arg3)
            :code '((ARGS 4)
                    (LVAR 0 0 ";" function-name)
                    (LVAR 0 1 ";" arg1)
                    (LVAR 0 2 ";" arg2)
                    (LVAR 0 3 ";" arg3)
                    (LISP-CALL-3)
                    (RETURN))))


  (set-global-var! '%lisp-apply
    (new-fn :name '%lisp-apply :args '(function-name args)
            :code '((ARGS 2)
                    (LVAR 0 0 ";" function-name)
                    (LVAR 0 1 ";" args)
                    (LISP-APPLY)
                    (RETURN))))

  ;; Primitive functions
  (dolist (prim *primitive-fns*)
     (set-global-var! (prim-symbol prim)
       (new-fn :name (prim-symbol prim)
               :code (seq (gen 'PRIM (prim-opcode prim))
                          (gen 'RETURN)))
       locale)))

;;; ==============================

(defparameter *par-t-top-level*
  '(call/cc (lambda (cc)
              (clear-all-choice-points)
              ;; (initialize-snark)
              ;; (use-resolution)
              ;; (use-paramodulation)
	      (letrec ((%result '())
                       (par-t (lambda ()
                                (newline)
                                (display "=> ")
                                (lset! %result ((compiler (read))))
                                (newline)
                                (write %result)
                                (par-t))))
                      (set! quit (lambda ()
                                   (cc %result)))
                      (par-t)))))

(defun comp-go (exp)
  "Compile and execute the expression."
  (run (compiler `(exit ,exp))))

(defun par-t (&key (load-examples t)
                   (load-standard-library t)
                   (thread-time-slice 50))
  "A compiled Par-T read-eval-print loop"
  (cond (load-examples
         (load-par-t-examples))
        (load-standard-library
         (load-par-t-standard-library)))
  (format t "~2&Welcome to the Par-T system!~%")
  (let ((*default-thread-time-slice* thread-time-slice))
    (comp-go *par-t-top-level*)))

;;;; Peephole Optimize-Bytecoder
;;; ============================

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
  (cl:assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))


;;; Support for the Par-T Reader
;;; ============================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'eof)
    (defconstant eof "EoF")))
(defun eof-object-p (x) (eq x eof))
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

;;; Loading files
;;; =============

(defvar *trace-par-t-reader* nil)

(defun load-par-t-file (file-name)
  (let ((result '())
        (*package* (find-package '#:parallel-thetis)))
    (with-open-file (stream file-name :direction :input)
      (do ((form (par-t-read stream) (par-t-read stream)))
	  ((eof-object-p form) (nreverse result))
        (when *trace-par-t-reader*
          (format *trace-output*
                  "~&Reading: ~:W~%" form)
          (force-output *trace-output*))
        (let ((evaluation-result (comp-go form)))
          (when (or (stringp evaluation-result) (symbolp evaluation-result))
            (push evaluation-result result)))))))

(defun par-t-system-file (name)
  (merge-pathnames
   (make-pathname :name name :type "poem")
   (directory-namestring (asdf:system-source-file :par-t))))

(defun load-par-t-standard-library (&key (print-heralds t))
  (let ((bootstrap-procs (par-t-system-file "bootstrap-procedures"))
        (macro-expander (par-t-system-file "macro-expander"))
        (macros (par-t-system-file "macros"))
        (stdlib (par-t-system-file "standard-library"))
        (snarky-t (par-t-system-file "snark-interface"))
        (object-system (par-t-system-file "objects")))
    (flet ((print-herald (list)
             (when print-heralds
               (format t "Loading ~A~%" (first list))
               (format t "Defined ~:W~%" (rest list))
               (force-output))))
      (print-herald (load-par-t-file bootstrap-procs))
      (print-herald (load-par-t-file macro-expander))
      (print-herald (load-par-t-file macros))
      (print-herald (load-par-t-file stdlib))
      (print-herald (load-par-t-file snarky-t))
      (print-herald (load-par-t-file object-system)))))

(defun load-par-t-compiler ()
  (let ((filename (par-t-system-file "compiler")))
    (load-par-t-file filename)))

(defun load-par-t-tests ()
  (initialize-top-level-locale)
  (load-par-t-standard-library)
  (let ((filename (par-t-system-file "tests")))
    (load-par-t-file filename)))

(defun load-par-t-examples ()
  (initialize-top-level-locale)
  (format *standard-output* "~&Loading Par-T Standard Library... ")
  (force-output *standard-output*)
  (load-par-t-standard-library :print-heralds nil)
  (format *standard-output* "done.~%")
  (let ((filename (par-t-system-file "examples")))
    (format *standard-output* "~&Loading Par-T Examples... ")
    (force-output *standard-output*)
    (load-par-t-file filename)
    (format *standard-output* "done.~%")
    (when (member :snarky-t *features*)
      (load-snarky-t-examples))))

(defun load-snarky-t ()
  (load-par-t-file (par-t-system-file "snark-interface")))

(defun load-snarky-t-examples ()
  (format *standard-output* "~&Loading Snarky-T Examples... ")
  (force-output *standard-output*)
  (load-par-t-file (par-t-system-file "snarky-examples"))
    (format *standard-output* "done.~%"))

;;; Set-up the compiler.
;;; ===================

;;; We call `init-par-t-com' after loading this file so that we don't
;;; have to remember calling it before we do something with the
;;; compiler in the toplevel.

(initialize-top-level-locale)
(load-par-t-standard-library)
(pushnew :par-t *features*)
(pushnew :snarky-t *features*)

;;; To allow quick testing after loading the system.
(defun cl-user::load-and-run-all-par-t-tests ()
  (let ((*package* (find-package '#:parallel-thetis)))
    (load-par-t-tests)
    (comp-go '(all-tests))))
