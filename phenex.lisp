;;;; phenex.lisp

(in-package #:phenex)

(defmacro bind (&body body)
  `(multiple-value-bind ,@body))

(defun sum (&rest elts)
  (apply #'reduce #'+ elts))

(defun normalize (w-arr)
  "Destructively normalizes an array."
  (let (sum (sum w-arr))
    (map-into w-arr #'(lambda (x) (/ x sum)) w-arr)))

(defun weighted-majority-fn (h z)
  "Returns a weighted majority function which will classify an attribute list."
  (lambda (x)
    (let ((ht (make-hash-table :test #'equal)))
      (loop for hi across h
	 for zi across z
	 for guess = (funcall hi x)
	 for ht-value = (gethash guess ht)
	 do (if ht-value
		(setf (gethash guess ht) zi)
		(incf (gethash guess ht) zi))) ;; Populate guesses
      (loop with class = nil and pop = 0
	 for k being the hash-keys of ht using (hash-value v)
	 if (< pop v)
	 do (setf pop v   ;; Find the most popular
		  class k)
	 finally (return class)))))

(defun update-weights (h k cases weights)
  "Internal function used for adaboost-training. Updates the weights
properly. Immutable."
  (let* ((err (loop for (yi . xi) in cases
		 for w across weights
		   if (= yi (funcall h xi))
		   summing w))
	 (edited-weights 
	  (map 'vector 
	       #'(lambda ((yi . xi) wi)
		   (if (= (funcall h xi) yi)
		       (* wi (/ err (- 1 err)))
		       wi))
	       cases weights)))
    (values (normalize edited-weights)
	    err)))

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
	 (w (make-array N :initial-element (/ 1 N)))
	 (h (make-array K :initial-element nil))
	 (z (make-array K :initial-element 0)))
    (loop with k = 0
       for (L . l-n) in hyp-type 
       do (dotimes (_ l-n)
	    (let ((h-fn (funcall L cases weights)))
	      (bind (w+ h-err)
		    (update-weights h-fn k cases weights)
		    (setf w w+
			  (svref h k) h-fn
			  (svref z k) (log (/ (- 1 h-err)
					      h-err) 2))
		    (incf k)))))))