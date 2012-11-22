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
Usage: phenex NBCs ID3s ID3d filter-type bucket-size train-p rand infile

All options are required, and mean the following:
    NBCs - Number of Naive Bayesian Filters
    ID3s - Number of ID3-trees
    ID3d - Depth of the ID3 trees (-1 for unbounded depth)
    filter-type - Either \"none\" or \"discretize\"
    bucket-size - Ignored if filter-type is \"none\", the size of buckets if
                  filter-type is \"discretize\"
    train-p - How much of the dataset which should be trained on. Not in
              percent, but in decimals. (E.g. 0.5 = 50%)
    rand - \"true\" or \"false\". When true, is actually random. If false, will
           be deterministic (i.e. return same result every time).
    infile - The input file with the data to work on.
"
  )


(defun main (argv)
  "Entry point for application."
  (when (/= (length argv) 9)
    (format t banner)
    (format t synopsis)
    (sb-ext:exit))
  (let ((nbcs (parse-integer (nth 1 argv)))
	(id3s (parse-integer (nth 2 argv)))
	(id3d (parse-integer (nth 3 argv)))
	(filter-type (nth 4 argv))
	(bucket-size (parse-integer (nth 5 argv)))
	(train-p (read-from-string (nth 6 argv)))
	(rand (nth 7 argv))
	(infile (nth 8 argv))
	filter dataset hs-lst)

    (if (string= rand "true")
	(setf *random-state* (make-random-state t)))
    (cond ((string= filter-type "discretize")
	   (setf filter #'(lambda (cases) (uniformly-discretise 
				      cases
				      bucket-size))))
	  ((string= filter-type "none")
	   (setf filter #'identity)))
    (setf dataset
	  (funcall filter
		   (-> infile
		       (read-dataset)
		       (shuffle))))
    (if (plusp id3s)
	(push (cons (id3+depth id3d) id3s) hs-lst))
    (if (plusp nbcs)
	(push (cons #'naive-bayes nbcs) hs-lst))

    (destructuring-bind (train test)
	(split-dataset dataset train-p)
      (destructuring-bind (h z)
	  (adaboost-training hs-lst train)
	(let ((total-fn (weighted-majority-fn h z))
	      (nbc (if (plusp nbcs)
		       (weighted-majority-fn (subseq h 0 nbcs)
					     (subseq z 0 nbcs))))
	      (id3 (if (plusp id3s)
		       (weighted-majority-fn (subseq h nbcs)
					     (subseq z nbcs)))))
	  (format t "~&TOTALS~%")
	  (format t "~&~D NBCs, ~D ID3s with a maximal depth of ~D~%"
		  nbcs id3s id3d)
	  (format t "~,5F wrong on training set~%"
		  (percent-wrong total-fn train))
	  (format t "~,5F wrong on test set~%" (percent-wrong total-fn test))
	  (format t "~,5F std.dev. on training set~%" (std-dev train h))
	  (format t "~,5F avg.dev. on training set~%" (avg-dev train h))
	  (when nbc
	    (format t "NBCs~%")
	    (format t "~,5F wrong on training set~%" (percent-wrong nbc train))
	    (format t "~,5F wrong on test set~%" (percent-wrong nbc test))
	    (format t "~,5F std.dev. on training set~%"
		    (std-dev train (subseq h 0 nbcs)))
	    (format t "~,5F avg.dev. on training set~%"
		    (avg-dev train (subseq h 0 nbcs))))
	  (when id3
	    (format t "ID3s~%")
	    (format t "~,5F wrong on training set~%" (percent-wrong id3 train))
	    (format t "~,5F wrong on test set~%" (percent-wrong id3 test))
	    (format t "~,5F std.dev. on training set~%"
		    (std-dev train (subseq h nbcs)))
	    (format t "~,5F avg.dev. on training set~%"
		    (avg-dev train (subseq h nbcs))))))))
  (sb-ext:exit))
