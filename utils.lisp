(in-package #:phenex)

(defconstant EPSILON 1e-9)

(defun append-1 (x y)
  "Adds the element y to the end of x."
  (append x (list y)))

(defmacro ->> (x &rest forms)
 "Threads the expr through the forms. Inserts x as the last item in the first
form. If there are more forms, inserts the first form as the last item in second
form, etc."
 (do ((up-next forms (cdr up-next))
      (res x (append-1 (car up-next) res)))
     ((null up-next) res)))

(defmacro bind (&body body)
  `(multiple-value-bind ,@body))

(defun sum (&rest elts)
  (apply #'reduce #'+ elts))
