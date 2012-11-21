;;;; phenex.asd

(in-package :cl-user)

(defpackage :phenex-asd
  (:use :cl :asdf))

(in-package :phenex-asd)

(defvar *phenex-version* "0.1.0-alpha01"
  "Current version of phenex as a string.")

(export '*phenex-version*)

(asdf:defsystem #:phenex
  :serial t
  :description 
  "A boosting program with diverse classifiers, like music to your ears."
  :author "Jean Niklas L'orange <jeannikl@hypirion.com>"
  :license "Eclipse Public License"
  :depends-on (#:split-sequence)
  :components ((:file "package")
	       (:file "utils")
	       (:file "dataset")
	       (:file "discretise")
	       (:file "adaboost")
	       (:file "naive-bayes")
	       (:file "id3")
               (:file "phenex")))
