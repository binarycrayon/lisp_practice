;; chapter 3 create a music db

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))


(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(defun fill-db ()
  (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
  (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
  (add-record (make-cd "Home" "Dixie Chicks" 9 t)))

(defun dump-db()
  ;; dolist macro binds each element to the variable cd
  (dolist (cd *db*)
    ;; in format, t is shorthand for stream
    ;; ~a directive stands for aesthetic directive
    ;; ~t directive is for tabulating
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  ;; *query-io* is a global variable
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db(filename)
  ;; with-open-file macro opens a file, binds the stream to
  ;; a variable, executes a set of expressions, and then closes
  ;; the file. Also make sure file is closed even if something wrong
  ;; while evaluating the body.
  (with-open-file (out filename
                       :direction :output
                       ;; specify opening the file for writing with :direction :output
                       ;; do print will write print outs to db
                       :if-exists :supersede
                       ;; overwrite an existing file of the same name if exists
                       )
    (with-standard-io-syntax
      ;; ensure certain variables that affect behavior of PRINT are
      ;; set to standard values
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
;; setf macro is main assignment operator.
;; setf set the first argument to the result of evaluating its second argument

;; (select :artist "Dixie Chicks")

(defun select (selector-fn)
  ;; selector-fn allows me to write several more functions, such
  ;; as select-by-title, select-by-rating, select-by-title-and-artists
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; crash course on keyword argument
;; (defun foo (a b c) (list a b c))
;; (defun foo (&key a b c) (list a b c))
;; difference is &key, with &key:
;; (foo :a 1 :b 2 :c 3) -> (1 2 3)
;; (foo :c 3 :b 2 :a 1) -> (1 2 3)
;; (foo :a 1 :c 3) -> (1 NIL 3)
;; (foo) -> (NIL NIL NIL)
;; provide default param value
;; (defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
;; (foo :a 1 :c 3) -> (1 20 3 T)

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
;; (if t 1) -> 1
;; (if nil 1 2) -> 2
;; can I adjust above to use 'or' ?

;; mapcar maps over a list, and return a new list containing results of
;; calling a function on each item of original list
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))
;; setf crash course: general assignment op, assign lots of "Places" than just variables
;; BUT, setf is irrelevant with getf

(defun delete-rows (selector-fn)
  ;; delete is a reserved keyword in clisp
  (setf *db* (remove-if selector-fn *db*)))
;; *db* will automatically garbage collected

;; The lisp macro system allows generating functions
;; you would use DEFMACRO instead of DEFUN. Example:
;; (defmacro backwards (expr) (reverse expr))
;; (backwards ("hello, world" t format)) would not evaluate ("hello, world" t format) just yet
;; instead it will eval backwards macro and turn the expression to (format t "hello world"), then pass that to REPL
;; and you get: hello, world

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;;
(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
;; ,@ splice into the middle of a list
;;`(and ,@(list 1 2 3) 4) -> (AND 1 2 3 4)

;; &rest is unlike &key which will parse the arguments into a list
;; (where :title "give us a break" :ripped t)
;; will assign clauses as (:title "give us a break" :ripped t)

;; Want to know what macro will generate? use macroexpand-1
;; (macroexpand-1 '(where :title "give us a break" :ripped t))
