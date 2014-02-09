;;;; language-extension.pattern-specializer.asd --- System definition for the language-extension.pattern-specializer system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language-extension.pattern-specializer-sytem
  (:use
   #:cl
   #:asdf))

(cl:in-package #:language-extension.pattern-specializer-sytem)

(defsystem :language-extension.pattern-specializer
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :license     "LLGPLv3; see COPYING file for details."
  :description "Use optima patterns as specializers in CLOS methods - SBCL ONLY"
  :depends-on  (;; (:feature :sbcl) this works differently than one might think; it's more like (:if-features :sbcl :foo :bar)

                :alexandria
                :optima)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "pcl-patch"
                               :if-feature :sbcl)
                              (:file       "package")
                              (:file       "optima-extensions")
                              (:file       "pattern-specializer")))))
