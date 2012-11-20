;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defun naive-bayes (train weights)
  "Takes a training set and a corresponding weight vector as input, and
generates a naive bayes classifier based on that data. If the naive bayesian
classifier is unable to determine what class a specific input is, it will take
the most frequent class and return that one instead. If there are multiple
classes with the highest weight, it will return the first class observed by the
naive bayes trainer from the training set."
  (let ((attr-count (length (cdar train)))
	(ht (make-hash-table :test 'equal))
	(classes nil)
	(most-frequent nil))
    (macrolet ((p (&rest lookup) `(gethash (list ,@lookup) ht 0)))
      (loop for (C . attrs) in train
	 for w across weights
	 do 
	   (progn
	     (incf (p C) w)
	     (pushnew C classes)
	     (loop for attr in attrs
		for attr-pos from 0
		do (progn
		     (incf (p c attr-pos) w)
		     (incf (p c attr-pos attr) w)))))
      (setq classes (sort classes #'(lambda (x y) (> (p x) (p y))))
	    most-frequent (first classes))
      (lambda (attrs)
	(loop with best-p = 0 and best = most-frequent ; in case noone fits
	   for C in classes
	   for prob = (* (p C)
			 (reduce #'* 
				 (mapcar 
				  #'(lambda (attr attr-pos)
				      (/ (p c attr-pos attr)
					 (p c attr-pos)))
				  attrs
				  (range 0 attr-count))))
	   if (< best-p prob)
	   do (setq best-p prob
		    best c)
	   finally (return best))))))
