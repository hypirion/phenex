;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: PHENEX; Base: 10 -*-

(in-package #:phenex)

(defconstant banner
"        _                          
  _ __ | |__   ___ _ __   _____  __
 | '_ \\| '_ \\ / _ \\ '_ \\ / _ \\ \\/ /
 | |_) | | | |  __/ | | |  __/>  < 
 | .__/|_| |_|\\___|_| |_|\\___/_/\\_\\
 |_|                               

")

(defconstant synopsis
"Phenex is a boosting program with diverse classifiers, like music to your ears.
Usage: phenex NBCs ID3s ID3d filter-type bucket-size train-p seed infile

All options are required, and mean the following:
    NBCs - Number of Naive Bayesian Filters
    ID3s - Number of ID3-trees
    ID3d - Depth of the ID3 trees (-1 for unbounded depth)
    filter-type - Either \"none\" or \"discretize\"
    bucket-size - ignored if filter-type is \"none\", the size of buckets if
                  filter-type is \"discretize\"
    train-p - How much of the dataset which should be trained on. Not in
              percent, but in decimals. (E.g. 0.5 = 50%)
    seed - Seed used for the randomization part. -1 for a \"truly randomized\"
           seed, otherwise set to the specified integer.
    infile - The input file with the data to work on.
"
  )


(defun main (argv)
  "Entry point for application."
  (when (/= (length argv) 9)
    (format t banner)
    (format t synopsis)
    (sb-ext:quit))
  (let ((nbcs (parse-integer (nth 1 argv)))
	(id3s (parse-integer (nth 2 argv)))
	(id3d (parse-integer (nth 3 argv)))
	(filter-type (nth 4 argv))
	(bucket-size (nth 5 argv))
	(train-p (read-from-string (nth 6 argv)))
	(seed (parse-integer (nth 7 argv)))
	(infile (nth 8 argv))))
  (sb-ext:quit))

(defun percent-right (fn dataset)
  (if (null dataset)
      1.0
      (loop with count = 0 and correct = 0
	 for (class . attrs) in dataset
	 do (incf count)
	 if (= class (funcall fn attrs))
	 do (incf correct)
	 finally (return (/ correct count)))))

(defun test-dataset (h-lst dataset-file buckets)
  (destructuring-bind (train test)
      (-> dataset-file
	  (read-dataset)
	  (shuffle)
	  (uniformly-discretise buckets)
	  (split-dataset 0.7))
    (let* ((ab (adaboost-training h-lst train))
	   (abfn (apply #'weighted-majority-fn ab)))
      (format t "~&~F~%" (percent-right abfn train))
      (format t "~&~F~%" (percent-right abfn test))
      ab)))
