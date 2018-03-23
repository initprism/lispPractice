(defun my-map (proc items)
    (if (null items)
              nil
	              (cons (funcall proc (car items))
			           (my-map proc (cdr items)))))

(defun my-filter (pred seq)
      (cond ((null seq) nil)
	       ((funcall pred (car seq))
		    (cons (car seq)
			     (my-filter pred (cdr seq))))
	          (t (my-filter pred (cdr seq)))))

(defun accumulate (op initial seq)
      (if (null seq) initial
	        (funcall op (car seq)
			    (accumulate op initial (cdr seq)))))

(defun enumerate-interval (low high)
      (if (> low high)
	        nil
		        (cons low (enumerate-interval (+ low 1) high))))

(defun flatmap (proc seq)
      (accumulate #'append nil (my-map proc seq)))

 (defun my-remove (item seq)
       (my-filter (lambda (x) (not (= x item)))
		          seq))

 (defun permutations (s)
       (if (null s) (list nil)
	         (flatmap (lambda (x)
			         (my-map (lambda (p) (cons x p))
					      (permutations (my-remove x s))))
			       s)))


(permutations `(1 2 3))
