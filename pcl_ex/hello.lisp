(defun hello-world()
  (format t "hello, world"))

(defun init-db()
  (defvar *db* (list nil)))

(defun add-record (cd)
  (push cd *db*))
