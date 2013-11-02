;;; Various utilities

;; Plotting
(defun plot (fn min max step)
	"A quick and dirty way to plot functions"
	(loop for i from min to max by step do
		(loop repeat (funcall fn i) do (format t "*"))
		(format t "~%")))
		
(defun fixedplot (fn)
	"An even dirtier way to plot functions (fixed min/max values, fixed step)"
	(plot fn 0 20 1))
	
;; Memoization (TODO: TEST)
(defun memoize (fn)
"A function to achieve memoization (performance improvements)"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind 
              (result exists)
            (gethash args cache)
          (if exists
              result
              (setf (gethash args cache)
                    (apply fn args)))))))