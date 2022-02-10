(cl:in-package #:asdf-user)

(defsystem "sanverter"
  :version (:read-file-form "version.lisp-expr")
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "Basic converter between SRT and ASS file format."
  :license "BSD 2-Clause License"
  :in-order-to ((test-op (load-op "sanverter-test")))
  :perform (test-op (o c) (symbol-call :sanverter-test :suite-tests))
  :depends-on ("claraoke" "esrap")
  :serial t
  :components
  ((:static-file "bad-source.ass")
   (:static-file "bad-source.srt")
   (:file "package")
   (:file "parser")
   (:file "printer")
   (:file "sanverter")))

