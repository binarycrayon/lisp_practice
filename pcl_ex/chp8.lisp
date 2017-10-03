;;; DEFMACRO DEFINITION FOR MAC READ ONLY


;; (defmacro when (condition &rest body)
;;   `(if ,condition (progn ,@body)))

;; Macro 'when' not known at the moment, awaiting input
(defun foo (x)
  (when (> x 10) (print 'big)))

;; when 'when' macro is executed, the form is expanded to:
;; (if (> x 10) (progn (print 'big)))

;; general form
;; (defmacro name (parameter*)
;;   "Optional documentation string."
;;   body-form*)

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; &rest and &body are semantically equivalent, &body are more used with macro
(defmacro do-primes-first (var-and-range &rest body)
  ;; no need to expand first, second and third var by hand, macro has destructing param list
  ;; this involves taking apart a structure
  ;; replace the statement with (var start end)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

;; so after code review:

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))
;; we get

;; (do-primes var-and-range &rest body)

;; and

;; (do-primes (var start end) &body body)

;; expand an execute do-primes macro
(do-primes (p 0 19) (format t "~d " p))

;; show macro expansion in SLIME
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

;;; Exercises
;; 8-1)  The do-primes macro could have been written like
;; (defmacro do-primes (var start end &rest body)
;;   Explain why the book's approach is better?

;;   8-2)  Write with-gensyms macro using mapcar.

;; 8-3)  Write do-primes using with-gensyms.
