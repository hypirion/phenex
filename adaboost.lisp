;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defconstant jiggle-rate 1.5)

(defun count-classes (cases)
  (let ((ht (make-hash-table :test #'equal)))
    (mapc 
     #'(lambda (x) (setf (gethash (car x) ht) t))
     cases)
    (hash-table-count ht)))

(defun normalize (w-arr)
  "Destructively normalizes an array."
  (let ((sum (sum w-arr)))
    (if (zerop sum)
	(normalize (map-into w-arr (constantly 1) w-arr))
	(map-into w-arr #'(lambda (x) (/ x sum)) w-arr))))

(defun weighted-majority-fn (h z)
  "Returns a weighted majority function which will classify an attribute list."
  (lambda (x)
    (let ((ht (make-hash-table :test #'equal)))
      (loop for hi across h
	 for zi across z
	 for guess = (funcall hi x)
	 do (incf (gethash guess ht 0) zi)) ;; Populate guesses
      (loop with class = nil and pop = 0
	 for k being the hash-keys of ht using (hash-value v)
	 if (< pop v)
	 do (setf pop v   ;; Find the most popular
		  class k)
	 finally (return class)))))

(defun jiggle-weights (w beta)
  "Internal function used for adaboost-training. Updates the weights
properly. Immutable."
  (let ((N (array-total-size w)))
    (->>
     (map 'vector
	  #'(lambda (wi)
	      (max 0 (+ wi (/ (- (random 2.0) 1)
			      (expt N beta)))))
	  w)
     (normalize))))

(defun update-weights (h cases weights L)
  "Internal function used for adaboost-training. Updates the weights
properly. Immutable."
  (let* ((err (loop for (yi . xi) in cases
		 for w across weights
		 if (not (= yi (funcall h xi)))
		 summing w))
	 (edited-weights
	  (cond ((>= err (/ (- L 1.0) L)) (jiggle-weights weights jiggle-rate))
		((< 0 err (/ (- L 1.0) L))
		 (map 'vector
		      #'(lambda (yx wi)
			  (destructuring-bind (yi . xi)
			      yx
			    (if (= (funcall h xi) yi)
				(* wi (/ err 
					 (max (- 1 err) EPSILON)))
				wi)))
		      cases weights))
		((zerop err) (jiggle-weights weights jiggle-rate)))))
    (values (normalize edited-weights)
	    (log (* (/ (- err 1) (if (zerop err) -1 err))
		    (- L 1)))))) ; $\log\left[\tfrac{1-error}{error}(L - 1)\right]$

(defun adaboost-training (hyp-type cases)
  "Defines the ADABOOST-TRAINING algorithm. Takes in a list of hypotheses and a
list of training cases, and returns a list of hyptotheses according to the
ADABOOST algorithm. Every element in the list of hypotheses must be on the
form (c-fn . n). c-fn is a classifier function creating a function of a specific
classifier, taking in two arguments: The list of cases along with a list of
weights. n is the number of hypotheses of that specific case we should
generate. Every element in cases is a list of the different attributes,
BEGINNING with the class it actually is. The list of hypotheses contains a
pair (h-fn . w), where w is how much weight a the hypothesis should be given."
  (let* ((N (length cases))
	 (K (sum hyp-type :key #'cdr))
	 (L (count-classes cases))
	 (w (make-array N :initial-element (/ 1.0 N)))
	 (h (make-array K :initial-element nil))
	 (z (make-array K :initial-element 0)))
    (loop with k = 0
       for (hyp-fn . l-n) in hyp-type 
       do (dotimes (_ l-n)
	    (princ ".")
	    (let ((h-fn (funcall hyp-fn cases w)))
	      (multiple-value-bind (w+ h-err)
		    (update-weights h-fn cases w L)
		    (setf w w+
			  (svref h k) h-fn
			  (svref z k) h-err)
		    (incf k)))))
    (list h z))) ;; cons h z
