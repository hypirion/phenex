;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defconstant banner
"        _                          
  _ __ | |__   ___ _ __   _____  __
 | '_ \\| '_ \\ / _ \\ '_ \\ / _ \\ \\/ /
 | |_) | | | |  __/ | | |  __/>  < 
 | .__/|_| |_|\\___|_| |_|\\___/_/\\_\\
 |_|                               ")

(defun main (argv)
  (format t "~A~%" banner)
  (sb-ext:exit))
