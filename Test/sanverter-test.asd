(cl:in-package #:asdf-user)

(defsystem "sanverter-test"
  :depends-on ("fiveam" "claraoke")
  :serial t
  :components
  ((:file "package")
   (:file "sanverter-tests")))

