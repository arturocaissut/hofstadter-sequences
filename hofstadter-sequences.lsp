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

;; TODO Make the next two functions macros
(defun hofstadter-q (n &optional (r 1) (s 4 #|2|#))
	"A macro to generate Hofstadter-Q sequences (given r and s parameters). The default case reverts to the Hofstadter U sequence (the original Q sequence)."
	;; Put a bunch of asserts here
	(if (<= 1 n s)
		1
	(+ (hofstadter-q (- n (hofstadter-q (- n r) r s)) r s) (hofstadter-q (- n (hofstadter-q (- n s) r s)) r s))))

;; NOTE: The generalized formula needs to be checked, sources unclear. Default values are good, however it's not usable for i,j values different than 0,1
(defun hofstadter-pinn (n &optional (i 0) (j 1 #|0|#))
	"A macro to generate Hofstadter-Pinn sequences (given i and j parameters). The default case reverts to the Hofstadter U sequence (the original Q sequence)."
	;; Put a bunch of asserts here
	(if (< n 3)
		1
	(+ (hofstadter-pinn #|(- n i |#(hofstadter-pinn (- n 1) i j)#|)|# i j) (hofstadter-pinn (- n (hofstadter-pinn (- n 2) i j) j) i j))))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
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
(defun test-all (&optional (n 15))
	(format t "Testing Hofstadter sequences")
	(format t "~%")
	(test-sequence #'hofstadter-g n)
	(format t "~%")
	(test-sequence #'hofstadter-h n)
	(format t "~%")
	(test-sequence #'hofstadter-u n)
	(format t "~%")
	(test-sequence #'hofstadter-q n)
	(format t "~%")
	(test-sequence #'hofstadter-f n)
	(format t "~%")
	(test-sequence #'hofstadter-m n)
	(format t "~%")
	(test-sequence #'hofstadter-conway n)
	(format t "~%")
	(test-sequence #'hofstadter-pinn n)
	(format t "~%"))

;; Write to a file
(defun write-test ()
(defvar n 50)
(with-open-file (str "sequence_test_output.txt"
					 :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str "Testing Hofstadter sequences")
	(format str "~%")
	(format str "Hofstadter G, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-g i)))	(format str "~%")
	(format str "Hofstadter H, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-u i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter Q, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-q i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter U, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-U i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter F, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-f i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter M, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-m i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter-Conway, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-conway i)))	(format str "~%")
	(format str "~%")
	(format str "Hofstadter Pinn, first ~d values: " n )
	(loop for i from 1 to n do (format str "~a "(hofstadter-pinn i)))	(format str "~%")
	(format str "~%")))