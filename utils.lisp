;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defconstant EPSILON 1e-9)

(defun append-1 (x y)
  "Adds the element y to the end of x."
  (append x (list y)))

(defmacro -> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the second item in the first
  form. If there are more forms, inserts the first form as the second item in
  second form, etc."
  (if (null forms)
      x
      (destructuring-bind ((ff . fr) . r) forms
	`(-> ,(list* ff x fr) ,@r))))

(defmacro ->> (x &rest forms)
 "Threads the expr through the forms. Inserts x as the last item in the first
form. If there are more forms, inserts the first form as the last item in second
form, etc."
 (do ((up-next forms (cdr up-next))
      (res x (append-1 (car up-next) res)))
     ((null up-next) res)))

(defun update-in (ht ks f &rest args)
  (destructuring-bind (k . r) ks
    (if (null r)
	(setf (gethash k ht) (apply f (gethash k ht) args))
	(let ((v (gethash k ht)))
	  (if (null v)
	      (let ((nht (make-hash-table :test #'equal)))
		(setf (gethash k ht) nht
		      v nht)))
	  (apply #'update-in v r f args)))))

(defun get-in (ht ks)
  (destructuring-bind (k . r) ks
    (if (null r)
	(gethash k ht)
	(get-in (gethash k ht) r))))

(defun fnil (f v)
  (lambda (x &rest ys) (apply f (if (null x) v x) ys)))

(defun +* (&rest elts)
  (apply (fnil #'+ 0) elts))

(defmacro bind (&body body)
  `(multiple-value-bind ,@body))

(defun sum (&rest elts)
  (apply #'reduce #'+ elts))

(defun range (a b)
  (loop for x from a below b collect x))

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
         collect (list key value)))))

(defun min-key (k xs)
  "Returns the x for which (k x), a number, is least."
  (reduce 
   #'(lambda (x1 x2)
     (if (< (funcall k x1) (funcall k x2)) x1 x2))
   xs))
