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
       collect (list a c p))))

(defun most-common-class (cases probs)
  "Return the most common class out of the different cases, based on their
probabilities."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (f . _) in cases
	  for p in probs
       do (incf (gethash f ht 0) p))
    (loop with best-c = 0 and best = nil
       for k being the hash-keys in ht using (hash-value v)
	 if (< best-c v) do (setq best-c v best k)
	 finally (return best))))

(defun id3-tree (depth cases probs rem-attrs) ;; Assumes positive amt. of cases.
  (let ((first-class (caar cases)))
    (cond ((or (zerop depth) (null rem-attrs)) ; If no attributes left, pick
	   (most-common-class cases probs))    ; most common class
	  ((every #'(lambda (c) (= first-class (car c))) cases)
	   first-class) ; If every class is equal, return that one.
	  (t 
	   (let* ((best-attr (min-key ; Find best attribute to split with
			      #'(lambda (n) (entropy n cases probs))
			      rem-attrs))
		  (rem-attrs* (remove best-attr rem-attrs))
		  (most-common (most-common-class cases probs)))
	     (list best-attr
		   most-common
		   (loop with ht = (make-hash-table :test #'equal)
		      for (aval c p) in (partition-by best-attr cases probs)
		      do (setf (gethash aval ht) 
			       (id3-tree (- depth 1) c p rem-attrs*))
		      finally (return ht))))))))

(defun id3-lookup (attrs tree)
  (if (atom tree) tree
      (destructuring-bind (attr most-common ht) tree
	(let ((new-tree (gethash (nth attr attrs) ht)))
	  (if new-tree
	      (id3-lookup attrs new-tree)
	      most-common)))))

(defun id3 (depth cases weights)
  (let* ((attrs (range 0 (length (cdar cases))))
	 (tree (id3-tree depth cases (coerce weights 'list) attrs)))
    #'(lambda (attrs)
	(id3-lookup attrs tree))))

(defun id3+depth (depth)
  (lambda (cases weights)
    (id3 depth cases weights)))
