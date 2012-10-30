;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(defpackage #:parallel-thetis
  (:use #:common-lisp)
  ;;; Lisp Functions
  (:shadowing-import-from #:snark #:assert)
  (:import-from #:snark-lisp 
                #:iff #:implies
                #:implied-by #:xor #:nand #:nor
                #:forall #:exists)
  (:export 
   #:compiler #:comp-show #:comp-go #:par-t
   #:init-par-t-comp
   #:load-par-t-file  #:par-t-system-file
   #:load-par-t-standard-library
   #:load-par-t-compiler
   #:load-par-t-tests
   #:load-par-t-examples)
  
  ;;; Par-T Procedures
  (:export
   ;; Builtins
   #:+ #:- #:* #:/ #:< #:> #:<= #:>= #:/= #:=
   #:eq? #:eqv? #:equal? #:not
   #:null? #:pair? #:boolean? #:symbol? #:procedure?
   #:number? #:vector? #:char? #:string?
   #:cons
   #:car #:car-setter! #:cdr #:cdr-setter! #:cadr
   #:spawn
   #:%allocate-instance #:%allocate-entity
   #:%instance-class #:%instance-proc
   #:instance-class-setter! #:instance-proc-setter!
   #:%instance?
   #:instance-ref #:%instance-setter!
   #:compiler #:name!

   ;; Bootstrap Procedures
   #:assq #:list #:map1 #:append2 #:as-par-t-boolean
   #:read #:write #:display #:newline #:random
   #:eof-object? #:gensym #:eval #:constant?
   #:expand-quasiquote
   ;; Standard Library:
   ;; Various functions
   #:else #:undefined #:identity
   #:error
   #:apply
   ;; List processing
   #:list?
   #:list*
   #:cddr #:caddr #:cdddr #:cadddr #:cddddr
   #:first #:second #:third #:fourth #:fifth
   #:sixth #:seventh #:eigth #:ninth
   #:list-ref #:starts-with #:rest #:rest2 #:last
   #:ass #:assv #:assq
   #:mem? #:memq? #:memv? #:member?
   #:union
   #:collect-if
   #:remove-duplicates
   #:partition-list #:gsort
   #:position-of
   #:any-null?
   #:any? #:every?
   #:map #:for-each
   #:remove #:getl
   #:length #:length=1
   #:reverse
   #:append
   #:map-append
   ;; Numbers
   #:mod
   #:even? #:odd?
   ;; Non-deterministic computation
   #:choice-points #:clear-all-choice-points
   #:fail #:create-choice-points #:amb)
  (:nicknames #:par-t #:pt))
