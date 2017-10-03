;;; chapter 6 code

;; (setf x 10) variable assignment

(defparameter *array* (make-array 4))
;;*ARRAY*
*array*
;; #(0 0 0 0)
(incf (aref *array* 3))
;;1

;; rotatef and shiftf
;; rotatef rotates values between places, e.g.
;; (rotatef a b)

;; shiftf is similar to rotatef but instead of rotating values it shifts them to the left

;; rotatef, shiftf like all modify macros, are executed exactly once

;; exercises:
;; 6-1)  What is the result of the following form?

;; (let ((a-list (let ((count 0))
;;                 (list
;;                  (lambda () (let ((count 3)) (incf count)))
;;                  (lambda () (decf count))
;;                  (lambda () count)))))
;;   (funcall (first a-list))
;;   (funcall (first a-list))
;;   (funcall (second a-list))
;;   (funcall (third a-list)))

;; 6-2)  What is the result of the following expressions?

;; (defvar *count* 100)
;; (defvar *count* 101)
;; *count*

;; 6-3)  What is the result of the following expressions?

;; (defparameter *count* 100)
;; (defparameter *count* 101)
;; *count*

;; 6-4)  How to change the value of a variable defined using DEFVAR?

;; 6-5)  Is this a valid code?  Why?

;; (defconstant +e+ 2.71)
;; (let ((+e+ 2.7)) (* 10 +e+))

;; 6-6) What is the result of the following expression?

;; (let ((a 10) (b 11) (c 12))
;;   (rotatef a b c) (list a b c))

;; 6-7)  What is the result of the following expression?

;; (let ((a 10) (b 11) (c 12))
;;   (shiftf a b c 100) (list a b c))
