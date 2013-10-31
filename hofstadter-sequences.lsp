;;; The Hofstadter Sequences (plus utilities)

;; TODO Write assert where needed

(defun hofstadter-g (n)
"Hofstadter G sequence"
  (if (< n 1)
      0
    (- n (hofstadter-g (hofstadter-g (- n 1))))))
    
(defun hofstadter-h (n)
"Hofstadter H sequence"
  (if (< n 1)
      0
    (- n (hofstadter-h (hofstadter-h (hofstadter-h (- n 1)))))))

(defun hofstadter-u (n)
"Hofstadter U sequence (The sequence formerly known as Q sequence)"
  (if (< n 3)
      1
    (+ (hofstadter-u (- n (hofstadter-u (- n 1)))) (hofstadter-u (- n (hofstadter-u (- n 2)))))))

;; TODO Write a macro to define the two sequences in one time
(defun hofstadter-f (n)
"Hofstadter Female sequence"
	(if (< n 1)
	  1
	(- n (hofstadter-m (hofstadter-f (- n 1))))))
	
(defun hofstadter-m (n)
"Hofstadter Male sequence"
	(if (< n 1)
	  0
	(- n (hofstadter-f (hofstadter-m (- n 1))))))

(defun hofstadter-conway (n)
"Hofstadter-Conway $10000 sequence"
	(if (< n 3)
	  1
	(+ (hofstadter-conway (hofstadter-conway (- n 1))) (hofstadter-conway (- n (hofstadter-conway (- n 1)))))))

;; TODO Make this work
#|(defmacro hofstadter-q (r s n)
	"A macro to generate Hofstadter-Q sequences (given r and s parameters)"
	;; Put a bunch of asserts here
	`(if (<= 1 ,n ,s)
		1
		(+ (hofstadter-q (- ,n hofstadter-q (- ,n ,r))) (hofstadter-q (- ,n (hofstadter (- ,n ,s) ))))))|#
	
;; Test functions
(defmacro test (seq &optional (n 15))
"A (somehow silly) macro to test the sequences"
	`(loop for i from 0 to ,n do (format t "~a ~d: ~d~%" ',seq i (,seq i))))

;; TODO Convert into a macro-writing macro	
(defun test-sequence (seq &optional (n 15))
	(format t "~a, first ~d values: " (cadr seq) n )
	(loop for i from 1 to n do (format t "~a "(funcall seq i)))
	(format t "~%"))

;; TODO Make this a macro and/or optimize it
(defun test-all (functions &optional (n 15))
	(format t "Testing Hofstadter sequences")
	(format t "~%")
	(mapc test-sequence functions)
	(test-sequence #'hofstadter-g n)
	(format t "~%")
	(test-sequence #'hofstadter-h n)
	(format t "~%")
	(test-sequence #'hofstadter-u n)
	(format t "~%")
	(test-sequence #'hofstadter-f n)
	(format t "~%")
	(test-sequence #'hofstadter-m n)
	(format t "~%")
	(test-sequence #'hofstadter-conway n)
	(format t "~%"))
