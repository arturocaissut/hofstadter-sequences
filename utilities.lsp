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