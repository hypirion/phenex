;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defun entropy (att-nr cases probs)
  "The entropy given if we partition on attribute number att-nr."
  (let ((ht (make-hash-table :test #'equal))
	(attr-sum (make-hash-table :test #'equal)))
    (loop for (class . attrs) in cases
       for a = (nth att-nr attrs)
       for p in probs
       do (update-in ht (list a class) #'+* p)
       do (incf (gethash a attr-sum 0) p))
    (loop for si being the hash-values in ht using (hash-key a)
       for ai = (gethash a attr-sum)
       summing 
	 (* ai
	    (loop for ti being the hash-values in si
		  for ui = (/ ti ai)
	       summing (- (* ui (log ui 2))))))))

(defun partition-by (att-nr cases probs)
  "Returns a lists with pairs (cases probs) partitioned by attribute att-nr."
  (let ((ht-c (make-hash-table :test #'equal))
	(ht-p (make-hash-table :test #'equal)))
    (loop for c in cases
       for a = (nth (+ att-nr 1) c)
       for p in probs
       do (push c (gethash a ht-c))
       do (push p (gethash a ht-p)))
    (loop for a being the hash-keys in ht-c using (hash-value c)
       for p = (gethash a ht-p)
       collect (list c p))))

(defun most-common-class (cases probs)
  "Return the most common class out of the different cases, based on their
probabilities."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (f . r) in cases
	  for p in probs
       do (incf (gethash (car c) ht 0) p))
    (loop with best-c = 0 and best = nil
       for k being the hash-keys in ht using (hash-value v)
	 if (< best-c v) do (setq best-c v best k)
	 finally (return best))))
#|
(defun id3 (cases probs rem-attrs) ;; Assumes positive amt. of cases.
  (let ((first-class (caar cases)))
    (cond ((null rem-attrs)
	   (most-common-class cases))
	  ((every #'(lambda (c) (= first-class (car c))))
	   first-class)
	  
	  )
    ))
|#
