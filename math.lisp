;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defun percent-wrong (fn dataset)
  (if (null dataset)
      1.0
      (loop with count = 0 and correct = 0
	 for (class . attrs) in dataset
	 do (incf count)
	 if (not (= class (funcall fn attrs)))
	 do (incf correct)
	 finally (return (/ correct count)))))

(defun avg (xs)
  (/ (reduce #'+ xs)
     (float (length xs))))

(defun std-dev (cases hs)
  (if (= 1 (length hs)) 0
      (let* ((pw (mapcar #'(lambda (h) (percent-wrong h cases)) 
			 (coerce hs 'list)))
	     (mean (avg pw)))
	(sqrt
	 (/ (reduce #'+ (mapcar #'(lambda (x) (expt (- x mean) 2)) pw))
	    (- (length hs) 1))))))

(defun avg-dev (cases hs)
  (if (= 1 (length hs)) 0
      (let* ((pw (mapcar #'(lambda (h) (percent-wrong h cases))
			 (coerce hs 'list)))
	     (mean (avg pw)))
	(/ (reduce #'+ (mapcar #'(lambda (x) (abs (- x mean))) pw))
	   (length hs)))))
