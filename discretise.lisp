;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defun uniformly-discretise (cases buckets)
  "Splits attributes into uniformly sized buckets space-wise. Sets the attribute
value to the number of the bucket it is put in, 0 -> lowest bucket, n-1 ->
highest bucket, etc. Crashes with no mercy if one attribute only has one value."
  (let ((buckets-1 (- buckets 1))
        min max delta)
    (loop for case in cases
       for maxes = (cdar cases) then (mapcar #'max maxes (cdr case))
       for mins = (cdar cases) then (mapcar #'min mins (cdr case))
       finally (setq min mins max maxes))
    (setq delta (mapcar #'- max min))
    (loop for case in cases
       collect (cons (car case)
                     (mapcar 
                      #'(lambda (attr min delta)
                          (let ((uneven (* buckets (/ (- attr min)
                                                      delta))))
                            (min buckets-1 (floor uneven))))
                      (cdr case)
                      min delta)))))
