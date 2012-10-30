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
#+debug-poem-vm
(declaim (optimize debug))
#+(and optimize-poem-vm (not debug-poem-vm))
(declare (optimize (speed 3) (safety 2) (compilation-speed 0) (space 1) (debug 0)))

(defstruct ret-addr fn pc env)

(defun top (stack) (first stack))

(defvar *trace-par-t-vm* nil)
(defvar *trace-par-t-global-loads* nil)

;;; Support for threads
;;; ===================

(defun print-thread-state (state stream depth)
  (declare (ignore depth))
  ;; This is necessary, since threads may appear on the stack, and they
  ;; contain references to their states, which contain references to the
  ;; stack, and threads may appear on the stack, and ...
  (let ((*print-circle* t))
    (print-unreadable-object (state stream :type t :identity t)
      (format stream "~%  Fn:~10T~A~%  Code:~10T~:W~%  ~
                        Pc:~10T~A~%  Env:~10T~:W~%  ~
                        Stack:~10T~:W~%  Locale:~10T~A~%  ~
                        #Args:~10T~A~%  Instr:~10T~A~%  ~
                        Thread:~10T~A"
              (thread-state-fn state)
              (thread-state-code state)
              (thread-state-pc state)
              (thread-state-env state)
              (thread-state-stack state)
              (locale-identifier (thread-state-locale state))
              (thread-state-n-args state)
              (thread-state-instr state)
              (thread-id (thread-state-thread state))))))

;;; The state of a single thread running on the VM.  This might be directly
;;; integrated into the VM, but having it in its own class gives us the
;;; possibility to easily add different kinds of threads (e.g., compiled to
;;; native code or Lua).
;;;
(defstruct (thread-state (:constructor
                             %make-thread-state
                             (fn code pc env stack locale n-args instr thread))
                         (:print-function print-thread-state))
  (fn nil :type (or fn pt-entity))
  (code nil)
  (pc 0 :type (integer 0))
  (env '())
  (stack '())
  (locale (top-level-locale))
  (n-args 0 :type (integer 0))
  (instr nil)
  (thread nil))

(defun make-thread-state (&key fn code (pc 0) env stack (locale (top-level-locale))
                               (n-args 0) instr thread)
  (cl:assert fn (fn)
             "Cannot create a thread-state without function.")
  (unless code
    (setf code (fn-code fn)))
  (%make-thread-state fn code pc env stack locale n-args instr thread))

(defun print-thread (thread stream depth)
  (declare (ignore depth))
  (let ((*print-circle* t))
    (print-unreadable-object (thread stream :type t :identity t)
      (format stream "~A~%  ~:W~%  ~:W"
              (thread-id thread)
              (thread-state thread)
              (thread-group thread)))))

(defgeneric thread-group (threadlike)
  (:documentation
   "Returns the thread group of a thread or state.")
  (:method ((state thread-state))
    (thread-group (thread-state-thread state))))

(defgeneric (setf thread-group) (new-group threadlike)
  (:documentation
   "Sets the thread group of a thread or state.")
  (:method (new-group (state thread-state))
    (setf (thread-group (thread-state-thread state)) new-group)))

;;; A thread, i.e., a single thread of execution.  This subsumes what SCEL
;;; calls process, but it is more general, since threads can optionally share
;;; their locales and so be more similar to threads with shared state that to
;;; processes.
;;; 
(defstruct (thread (:constructor
                       %make-thread
                       (id parent %group blocked-p priority weight detached-p))
                   (:print-function print-thread))
  ;; A unique identifier for this thread.
  (id (gensym "THREAD-"))
  ;; The THREAD-STATE for this thread.  If we ever introduce threads that can
  ;; run on a different kind of VM this slot should be moved to a subclass.
  (state nil :type (or null thread-state))
  ;; The THREAD that spawned this thread, or NIL if it is detached or a
  ;; top-level thread.
  (parent nil :type (or null thread))
  ;; The THREAD-GROUP to which this thread belongs.
  (%group nil :type (or null thread-group))
  ;; NIL if this thread can currently execute, true if it is blocked.  Blocked
  ;; threads should set this field to a data structure that allows the
  ;; scheduler to test whether the thread has become executable again.
  ;; Details how this is supposed to work will be worked out later.
  (blocked-p nil)
  ;; The priority.  Threads withi higher priority should be preferred by the
  ;; scheduler to ones with lower priority.
  (priority 0 :type integer)
  ;; Weight is initially the priority, but can be changed by the scheduler for
  ;; to implement scheduling strategies.
  (weight 0 :type integer)
  ;; If true, the thread can be immediately collected after it has terminated
  ;; because nobody can wait for it.
  (detached-p nil :type (or null thread))
  ;; True if the thread has finished executing.
  (completed-p nil)
  ;; The result of the thread.  Only meaningful if completed-p is true.
  (result nil))

(defmethod thread-group ((thread thread))
  (thread-%group thread))
(defmethod (setf thread-group) (new-group (thread thread))
  ;; TODO: Should probably at least warn when overwriting an existing thread
  ;; group?
  (setf (thread-%group thread) new-group))

(defun print-thread-group (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity t)
    (format stream "~%  Main Thread: ~A~%   Threads: ~:W"
            (thread-id (thread-group-main-thread group))
            (mapcar 'thread-id (thread-group-threads group)))))

;;; A group of threads, controlled by a single scheduler.
;;; 
(defstruct (thread-group (:constructor
                             %make-thread-group
                             (main-thread threads scheduler scheduler-info))
                         (:print-function print-thread-group))
  ;; The main thread of this group.  This is the thread whose result is
  ;; returned by the VM and also the thread whose end is responsible for
  ;; quitting the VM.  The MAIN-THREAD must also be included in THREADS.
  main-thread
  ;; The threads in this group
  threads
  ;; The scheduler responsible for scheduling threads in this group
  scheduler
  ;; Data needed by the scheduler to make scheduling decisions.  Its precise
  ;; format depends on the scheduler.
  scheduler-info)

;;; Schedulers
;;; ----------

(defvar *default-thread-time-slice* most-positive-fixnum)

(defgeneric schedule (scheduler thread-group)
  (:documentation
   "Returns the thread of THREAD-GROUP that should be run in the next time
   slice or NIL, if no thread can currently run, and, as second value, the
   number of ticks the thread should run."))

(defgeneric initialize-scheduler (scheduler thread-group)
  (:documentation
   "Called before SCHEDULE is called for the first time on SCHEDULER; the main
purpose is to allow the scheduler to set up the SCHEDULER-INFO slot in
THREAD-GROUP."))

(defgeneric schedule-new-thread (scheduler thread-group new-thread)
  (:documentation
   "Called when NEW-THREAD is spawned for THREAD-GROUP."))

(defclass round-robin-scheduler ()
  ())

(defmethod schedule ((self round-robin-scheduler) (group thread-group))
  (let* ((index (thread-group-scheduler-info group))
         (thread (nth index (thread-group-threads group)))
         (ticks *default-thread-time-slice*))
    (cond (thread
           (cond ((not (thread-blocked-p thread))
                  (incf (thread-group-scheduler-info group))
                  (values thread ticks))
                 (;; There is at least one runnable thread; continue
                  ;; scheduling.  But update the SCHEDULER-INFO first.
                  (some (lambda (thread)
                          (not (thread-blocked-p thread)))
                        (thread-group-threads group))
                  (incf (thread-group-scheduler-info group))
                  (schedule self group))))
          (t
           ;; We arrived here because the index for the next scheduled thread
           ;; was out of range.  This is either because we have completed a
           ;; cycle through the theads or because somebody removed threads
           ;; from the group.
           (setf (thread-group-scheduler-info group) 0)
           (schedule self group)))))

(defmethod initialize-scheduler ((self round-robin-scheduler) (group thread-group))
  (declare (ignore self))
  (setf (thread-group-scheduler-info group) 0))

(defmethod schedule-new-thread ((scheduler round-robin-scheduler)
                                (group thread-group)
                                (new-thread thread))
  (declare (ignore scheduler))
  (let ((threads (thread-group-threads group)))
    (if (member new-thread threads)
        (warn "Scheduling thread ~A, which is already a member of group ~A."
              new-thread group)
        (setf (thread-group-threads group)
              (append threads (list new-thread))))))
                                    
                                 

;;; Constructors
;;; ------------

(defun make-thread-group (&key main-thread threads scheduler scheduler-info)
  (cond ((not scheduler)
         (setf scheduler (make-instance 'round-robin-scheduler)))
        ((symbolp scheduler)
         (setf scheduler (make-instance scheduler))))
  (%make-thread-group main-thread threads scheduler scheduler-info))

(defun make-thread (&key (id (gensym "THREAD-"))
                         (fn nil)
                         (parent nil)
                         (group nil)
                         (blocked-p nil)
                         (priority 0)
                         (detached-p nil))
  (when (eql parent *false*)
    (setf parent nil))
  (let ((result (%make-thread id parent group blocked-p priority priority detached-p)))
    (setf (thread-state result)
          (make-thread-state :fn fn :thread result))
    (unless group
      (progn 
        (setf group
              (if parent
                  (thread-group parent)
                  (progn
                    (make-thread-group :main-thread result
                                       :threads '()))))
        (initialize-scheduler (thread-group-scheduler group) group)))
    (setf (thread-group result) group)
    (schedule-new-thread (thread-group-scheduler group) group result)
    result))

(defun spawn-thread (fn parent args)
  "The function called by the VM to spawn a new thread."
  (apply #'make-thread :fn fn :parent parent args))

;;; Utility Functions
;;; =================

#+optimize-poem-vm
(declaim (inline set-up-call))

(defun set-up-call (state fun call-n-args)
  (cond ((fn-p fun)
         (setf (thread-state-fn state) fun
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
                  (setf (thread-state-fn state) real-fun
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
            (fn-name (thread-state-fn state)))
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

#+optimize-poem-vm
(declaim (inline run-thread-1-step))

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
                  #+debug-poem-vm
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
      (THREAD
       (push (thread-state-thread state) (thread-state-stack state))
       (values t nil nil 'thread))
      
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
                            :fn (thread-state-fn state)
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
                   (setf (thread-state-fn state) fun
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
                (let ((new-bindings (make-array n-args))
                      (stack (thread-state-stack state)))
                  (push new-bindings (thread-state-env state))
                  (loop for i from (- n-args 1) downto 0 do
                           (setf (elt new-bindings i) (pop stack)))
                  (setf (thread-state-stack state) stack))
                (values t nil nil 'args)))))
      (VARARGS
       (let* ((instr (thread-state-instr state))
              (n-req (arg1 instr))
              (n-args (thread-state-n-args state)))
         (cond ((< n-args n-req)
                (values nil t nil (list 'bad-varargs n-req n-args)))
               (t
                (let ((new-bindings (make-array (+ 1 n-req) :initial-element '()))
                      (stack (thread-state-stack state)))
                  (push new-bindings (thread-state-env state))
                  (loop repeat (- n-args n-req) do
                           (push (pop stack) (elt new-bindings n-req)))
                  (loop for i from (- n-req 1) downto 0 do
                           (setf (elt new-bindings i) (pop stack)))
                  (setf (thread-state-stack state) stack))
                (values t nil nil 'varargs)))))
      
      (FN
       (push (make-fn :code (fn-code (arg1 (thread-state-instr state)))
                      :name (fn-name (arg1 (thread-state-instr state)))
                      :args (fn-args (arg1 (thread-state-instr state)))
                      :env (thread-state-env state)) 
             (thread-state-stack state))
       (values t nil nil 'fn))
      
      (LISP-CALL
       (let ((args (pop (thread-state-stack state)))
             (fun (fdefinition (pop (thread-state-stack state)))))
         (push (apply fun args) (thread-state-stack state)))
       (values t nil nil 'lisp-call))
      
      (PRIM
       (let ((fun (fdefinition (arg1 (thread-state-instr state))))
             (n-args (thread-state-n-args state))
             (args '())
             (stack (thread-state-stack state)))
         (dotimes (i n-args)
           (push (pop stack) args))
         (push (apply fun args) stack)
         (setf (thread-state-stack state) stack))
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
      ((%INSTANCE-SETTER %INSTANCE-PROC-SETTER
                         SPAWN-THREAD)
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

#+optimize-poem-vm
(declaim (inline run-thread))

(defun run-thread (state &key (locale (top-level-locale))
                              (ticks *default-thread-time-slice*))
  "Run the abstract machine on the code for f."
  #+debug-poem-vm
  (check-type state thread-state)
  (dotimes (tick ticks (values :time-slice-exhausted nil))
    #+debug-poem-vm
    (cl:assert (< (thread-state-pc state) (length (thread-state-code state))) ()
               "Program counter out of range.")
    (setf (thread-state-instr state)
          (elt (thread-state-code state) (thread-state-pc state)))
    #+debug-poem-vm
    (print-trace-information state)
    (incf (thread-state-pc state))
    (multiple-value-bind (continue? error? value reason)
        (run-thread-1-step state locale)
      (unless continue?
        (if error?
            (error "Runtime error: ~A~%  State: ~:W" reason state)
            (return-from run-thread (values :done value)))))))

(defun run-loop (group thread state locale ticks)
  (let ((value :no-value)
        (scheduler (thread-group-scheduler group)))
    (loop
      (multiple-value-bind (exit-status thread-value)
          (run-thread state :locale locale :ticks ticks)
        (ccase exit-status 
          (:done
           (setf (thread-completed-p thread) t
                 (thread-blocked-p thread) t)
           (when (eq thread (thread-group-main-thread group))
             (setf value thread-value)))
          (:blocked
           (setf (thread-blocked-p thread) t))
          (:time-slice-exhausted)))
      (multiple-value-bind (new-thread scheduler-ticks)
          (schedule scheduler group)
        (cond (new-thread
               (setf thread new-thread
                     state (thread-state new-thread))
               (when scheduler-ticks
                 (setf ticks scheduler-ticks)))
              (t
               (return-from run-loop value)))))))

(defgeneric run (executable &key locale ticks)

  (:method ((fn fn) &key (locale (top-level-locale))
                         (ticks *default-thread-time-slice*))
    (let* ((thread (make-thread :fn fn))
           (state (thread-state thread))
           (group (thread-group thread)))
      (run-loop group thread state locale ticks)))

  (:method ((thread thread) &key (locale (top-level-locale))
                                 (ticks *default-thread-time-slice*))
    (run (thread-group thread) thread (thread-state thread) locale ticks))

  (:method ((group thread-group) &key (locale (top-level-locale))
                                      (ticks *default-thread-time-slice*))
    (multiple-value-bind (thread scheduler-ticks)
        (schedule (thread-group-scheduler group) group)
      (cond (thread
             (when scheduler-ticks
               (setf ticks scheduler-ticks))
             (run-loop group thread (thread-state thread) locale ticks))
            (t :no-thread))))

  (:method ((state thread-state) &key (locale (top-level-locale))
                                      (ticks *default-thread-time-slice*))
    (let* ((thread (thread-state-thread state))
           (group (thread-group thread)))
      (run-loop group thread state locale ticks))))

