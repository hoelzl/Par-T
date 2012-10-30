;;;; snark-t.asd

(asdf:defsystem #:snarky-t
  :serial t
  :description "The Snark interface for Parallel-Thetis"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on  (#:par-t #:snark)
  :components ((:file "snark-loader")))
  
