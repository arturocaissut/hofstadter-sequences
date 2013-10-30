;;; The Hofstadter Sequences (plus utilities)

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

(defun hofstadter-q (n)
"Hofstadter Q sequence"
  (if (< n 3)
      1
    (+ (hofstadter-q (- n (hofstadter-q (- n 1)))) (hofstadter-q (- n (hofstadter-q (- n 2)))))))

;; Test functions
(defmacro run-test (seq n)
"A (somehow silly) macro to test the sequences"
`(loop for i from 0 to ,n do (format t "~a ~d: ~d~%" ',seq i (,seq i))))
