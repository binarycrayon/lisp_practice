;;;Chapter 7 Macros: standard control constructs

;;; WHEN and UNLESS

;; (if (spam-p current-message)
;;     (file-in-spam-folder current-message)
;;     (update-spam-database current-message))
;; unable to execute both statement since the last one treated as else-clause

;; this works:
;; (if (spam-p current-message)
;;     (progn
;;       (file-in-spam-folder current-message)
;;       (update-spam-database current-message)))

;; or:
;; (when (spam-p current-message)
;;   (file-in-spam-folder current-message)
;;   (update-spam-database current-message))

(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;;; COND
;; (cond
;;   (test-1 form*)
;;   ..
;;   (test-N form*))

;;; DOLIST and DOTIMES

;; (dolist (var list-form)
;;   body-form*)

(dolist (x '(1 2 3 4 5))
  (print x)
  (if (evenp x) (return)))

;; (dotimes (var count-form)
;;   body-form*)

(dotimes (x 20) (print x) (if (evenp x) (return)))

;;; DO

;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   statement*)

;; fibonacci
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

;; loop to 4
;; (do ((i 0 (1+ i)))
;;     ((>= i 4))
;;   (print i))
;;
;; is same as
;;
;; (dottimes (i 4) (print i))

;; binds no variables in DO loop, loop while current time
;; is before some future date, print "Waiting" once a minute
(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "Waiting~%")
  (sleep 60))


;;; LOOP
;; (loop
;;       body-form*)

;; until that date, print waiting every 1 second
;; (loop
;;   (when (> (get-universal-time) *some-future-date*)
;;     (return))
;;   (format t "Waiting ...~%")
;;   (sleep 1))

;; review
;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   (body-form*))
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

;; find all vowels in a sentence
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))

;; sum first 10 squares
(loop for x from 1 to 10 summing (expt x 2))

;; summing, across, counting, finally, for, from, then, to are loop keywords
;; and can be extended with macros

;;; Exercises
;; -1)  Write the COND macro using what you've learned so far.  (Hint:  use WHEN)

;; 7-2)  What is the value of the following form?

;; (let ((x 10))
;;   (cond
;;     ((let ((x 11))
;;        (= x 10)) 100)
;;     ((= x 10) x)))

;; 7-3)  What is the value of this form?

;; (dolist (x '(10 20 30))
;;   (setf x (+ x x)))

;; 7-4)  Compute the 8th number of this series using DO: f(1)=1, f(2)=2, f(3)=3, f(n)=f(n-1)+f(n-2)+f(n-3)
