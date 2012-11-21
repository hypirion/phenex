;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defun read-dataset (pname)
  "Reads a dataset from a comma-separated value file, where the classification
is the last element in each line."
  (with-open-file (in pname)
    (do ((line (read-line in nil)
	       (read-line in nil))
	 (dataset nil))
	((null line) dataset)
      (push
       (->> line
	    (split-sequence #\,)
	    (mapcar #'read-from-string)
	    (nreverse))
       dataset))))

(defun shuffle (list)
  "Destructively shuffles a list."
  (sort list
	#'(lambda (x y)
	    (declare (ignore x y))
	    (zerop (random 2)))))

(defun split-dataset (list p)
  "Splits the dataset into two lists. The first list is p of the size of the
total list length rounded to the closest integer. The second is the remaining
elements."
  (let ((l (round (* p (length list)))))
    (list (subseq list 0 l)
	  (subseq list l))))
