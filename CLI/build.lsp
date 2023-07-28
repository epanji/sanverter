;;;;
;;;; This file will be called from Makefile
;;;;
(require :asdf)

(asdf:load-system "sanverter")

(asdf:make-build "sanverter"
                 :type :static-library
                 :move-here "./"
                 :monolithic t
                 :init-name "init_sanverter")

(compile-file "sanverter.lsp" :system-p t)

(c:build-program "sanverter"
                 :lisp-files
                 '("sanverter--all-systems.a"
                   "sanverter.o"))

