;;;; odysseus.asd

(asdf:defsystem #:par-t
  :serial t
  :description "The Parallel-Thetis compiler for Poem"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on () #+(or) (#:alexandria)
  :components ((:file "packages")
	       (:file "utilities")
	       (:file "vm")
	       (:file "bootstrap-compiler")))
  
