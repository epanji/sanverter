(cl:in-package #:asdf-user)

#-abcl (error "This is targetting JVM (Java Virtual Machine)")

(defsystem "sanverter-java"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "Basic converter between SRT and ASS file format."
  :depends-on ("sanverter")
  :serial t
  :components
  ((:file "packages")
   (:file "swing")
   (:file "icon")
   (:file "main")))

