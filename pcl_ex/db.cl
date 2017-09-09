(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))


(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(defun fill-db ()
  (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
  (add-record (make-cd "Fly" "Dexie Chicks" 8 t))
  (add-record (make-cd "Home" "Dixie Chicks" 9 t)))

(defun dump-db()
  ;; dolist macro binds each element to the variable cd
  (dolist (cd *db*)
    ;; in format, t is shorthand for stream
    ;; ~a directive stands for aesthetic directive
    ;; ~t directive is for tabulating
    (format t "~{~a:~10t~a~%~}~%" cd)))


