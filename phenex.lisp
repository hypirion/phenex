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
;	  (format t "~&TOTALS~%")
;	  (format t "~&~F~%" (percent-wrong total-fn train))
	  (format t "~&~D \\& ~D:~D &" nbcs id3s id3d)
	  (format t "~,5F &" (percent-wrong total-fn test))
;	  (format t "~&~F~%" (std-dev train h))
;	  (format t "~&~F~%" (avg-dev train h))
	  (when nbc
;	    (format t "NBCs~%")
;	    (format t "~&~F~%" (percent-wrong nbc train))
;	    (format t "~&~F~%" (percent-wrong nbc test))
	    (format t "~,5F &" (std-dev train (subseq h 0 nbcs)))
	    (format t "~,5F &" (avg-dev train (subseq h 0 nbcs))))
	  (if (not nbc)
	      (format t "- & - &"))
	  (when id3
;	    (format t "ID3s~%")
;	    (format t "~&~F~%" (percent-wrong id3 train))
;	    (format t "~&~F~%" (percent-wrong id3 test))
	    (format t "~,5F &" (std-dev train (subseq h nbcs)))
	    (format t "~,5F \\\\~%" (avg-dev train (subseq h nbcs))))
	  (if (not id3)
	      (format t "- & - \\\\~%"))))))
  (sb-ext:exit))


(defun avg (xs)
  (/ (reduce #'+ xs)
     (float (length xs))))

(defun std-dev (cases hs)
  (if (= 1 (length hs)) 0
      (let* ((pw (mapcar #'(lambda (h) (percent-wrong h cases)) 
			 (coerce hs 'list)))
	     (mean (avg pw)))
	(sqrt
	 (/ (reduce #'+ (mapcar #'(lambda (x) (expt (- x mean) 2)) pw))
	    (- (length hs) 1))))))

(defun avg-dev (cases hs)
  (if (= 1 (length hs)) 0
      (let* ((pw (mapcar #'(lambda (h) (percent-wrong h cases))
			 (coerce hs 'list)))
	     (mean (avg pw)))
	(/ (reduce #'+ (mapcar #'(lambda (x) (abs (- x mean))) pw))
	   (length hs)))))

(defun percent-wrong (fn dataset)
  (if (null dataset)
      1.0
      (loop with count = 0 and correct = 0
	 for (class . attrs) in dataset
	 do (incf count)
	 if (not (= class (funcall fn attrs)))
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
      (format t "~&~F~%" (percent-wrong abfn train))
      (format t "~&~F~%" (percent-wrong abfn test))
      ab)))
