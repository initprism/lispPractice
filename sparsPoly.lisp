(defvar *max-terms* 101)
(defparameter *terms* (make-array *max-terms*))
(defparameter *As* 0)
(defparameter *Ae* 0)
(defparameter *Bs* 0)
(defparameter *Be* 0)
(defparameter *Cs* 0)
(defparameter *Ce* 0)

(defstruct terms
    (coef nil)
    (expon nil))

(defun split-str (string &optional (separator " "))
  (labels ((split-help (string &optional (separator " ") (r nil))
	     (let ((n (position separator string
				:from-end t
				:test #'(lambda (x y)
					  (find y x :test #'string=)))))
	       (if n
		   (split-help (subseq string 0 n)
			       separator
			       (cons(subseq string (+ 1 n)) r))
		   (cons string r)))))
    (split-help string separator)))

(defun string-to-list (str)
  (if (not (streamp str))
      (string-to-list (make-string-input-stream str))
      (if (listen str)
	  (cons (read str) (string-to-list str))
	  nil)))

(defun compare (expon1 expon2)
  (cond ((> expon1 expon2) #\>)
	((= expon1 expon2) #\=)
	(t #\<)))

(defun clear-terms ()
  (setf *terms* (make-array *max-terms*))
  (setf *As* 0)
  (setf *Ae* 0)
  (setf *Bs* 0)
  (setf *Be* 0)
  (setf *Cs* 0)
  (setf *Ce* 0))

(defun attach (coef expon)
  (if (> *Ce* *max-terms*)
      (progn (princ "too many terms") (exit))
      (progn (setf (aref *terms* *Ce*) (make-terms :coef coef :expon expon))
	     (incf *Ce*)
	     (aref *terms* (- *Ce* 1)))))

(defun poly-add ()
	   (loop while (and (<= *As* *Ae*) (<= *Bs* *Be*))
	      do (let ((op (compare (terms-expon (aref *terms* *As*))
				     (terms-expon (aref *terms* *Bs*)))))
		    (cond ((eq op #\>)
			   (attach (terms-coef (aref *terms* *As*))
				   (terms-expon (aref *terms* *As*)))
			   (incf *As*))
			  ((eq op #\=)
			   (let ((coef (+ (terms-coef (aref *terms* *As*))
					  (terms-coef (aref *terms* *Bs*)))))
			     (when (not (zerop coef))
			       (attach coef (terms-expon (aref *terms* *As*))))
			     (incf *As*)
			     (incf *Bs*)))
			  ((eq op #\<)
			   (attach (terms-coef (aref *terms* *Bs*))
				   (terms-expon (aref *terms* *Bs*)))
				   (incf *Bs*)))))
	   (loop for x from *As* to *Ae*
	      do (attach (terms-coef (aref *terms* x)) (terms-expon (aref *terms* x))))
	   (loop for x from *Bs* to *Be*
	      do (attach (terms-coef (aref *terms* x)) (terms-expon (aref *terms* x))))
	   (decf *Ce*)
	   (loop for x from *Cs* to *Ce*
	      do (if(zerop (terms-expon (aref *terms* x)))
		    (format t "~s" (terms-coef (aref *terms* x)))
		    (format t "~sx^~s + "
			    (terms-coef (aref *terms* x))
			    (terms-expon (aref *terms* x))))))

(defun enter-poly (prefix As Ae Bs Be Cs Ce)
  (format t "Please type in ~(~a~) polynomial :" prefix)
  (let* ((p2 (read-line))
	 (poly (split-str
	       (coerce
		(remove nil(subst nil #\ (coerce p2 'list)))
		'string) "+"))
	 (coef (make-array 0 :element-type 'character
			     :fill-pointer 0
		      	     :adjustable t))
	 (expon (make-array 0
			     :element-type 'character
			     :fill-pointer 0
			     :adjustable t))
	 (p-count (symbol-value As)))
    (mapcar #'(lambda (x)
		(let ((len (- (length x) 1)))
		      (loop for i from 0 to len 
			 if (equal #\^ (aref x i))
			    do(progn(loop for r from 0 to (- i 2)
				   do(vector-push-extend (aref x r) coef))
				    (loop for z from (+ i 1) to len
				   do(vector-push-extend (aref x z) expon)))
			    else do(if(and(equal #\x (aref x i)) (= i len))
				      (progn (loop for r from 0 to (- i 1)
					     do(vector-push-extend (aref x r) coef))
					     (vector-push-extend #\1 expon))
				      (if(not(or(member #\x (coerce x 'list))
						 (member #\^ (coerce x 'list))))
					 (progn(loop for r from 0 to len
					     do(vector-push-extend (aref x r) coef))
					       (vector-push-extend #\0 expon)
					       (return)))))
		      (setf (aref *terms* p-count) 
			    (make-terms :coef (parse-integer coef)
				        :expon (parse-integer expon)))
		      (incf p-count)
		      (setf (fill-pointer coef) 0)
		      (setf (fill-pointer expon) 0)))
	    poly)
    (set Ae (1- p-count))
    (set Bs  p-count)
    (set Be (1- p-count))
    (set Cs p-count)
    (set Ce (symbol-value Cs))))

(defun sparse-poly-add ()
  (do ((answer nil))
      (nil)
    (format t "~&-----1 is calculate two poly-----")
    (format t "~&-----2 is exit-------------------")
    (format t "~&PLEASE TYPE IN A NUMBER :")
    (setf answer (read))
    (cond ((= 1 answer)
	   (clear-terms)
	   (enter-poly 'first '*As* '*Ae* '*Bs* 'null 'null 'null)
	   (enter-poly 'second '*Bs* 'null 'null '*Be* '*Cs* '*Ce*)
	   (poly-add)
	   (format t "~%~%"))
	  ((= 2 answer) (return))
	  (t (format t "NOT COLLECT INPUT~%~%")))))

