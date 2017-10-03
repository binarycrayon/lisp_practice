;;;; Chapter 5

;; Anonymous Functions
;; (lambda (parameters) body)

(funcall #'(lambda (x y) (+ x y)) 2 3)

;; ((lambda (x y) (+ x y)) 2 3)

(defun times-double (x) (* 2 x))


(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))

(plot #'times-double 0 10 1)

(plot #'(lambda (x) (* 2 x)) 0 10 1)

