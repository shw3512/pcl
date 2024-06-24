;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;100

;;;; CD database

;;; Code based on Chapter 3 of Peter Seibel's "Practical Common Lisp", freely available at
;;;     https://gigamonkeys.com/book/practical-a-simple-database
;;; Remarks.
;;; 1.  I have implemented a few minor edits from Seibel's code presented in the book.
;;; 2.  Code comments are set to verbose. (This project is intended for Lisp learners, not pros.)
;;;     Note to student: If desired, you could run this file through a filter that retains only
;;;     those lines that begin with a (one or more) semicolon, then try to complete the code
;;;     yourself; the comments will serve as a guide.

;;; Prepared by : shw
;;; Last updated: 2024-06-23



;;; Define function `make-cd` to make CD object, implemented as a plist (property list)
;;; 
;;; Keyword symbols (think, names of fields) in CD:
;;; - :title: str
;;;     - Title of CD
;;; - :artist: str
;;;     - Artist
;;; - :rating: int [0, 10]
;;;     - User rating of CD
;;; - :is-ripped: bool
;;;     - Whether CD has been ripped

(defun make-cd (title artist rating is-ripped)
  (list :title title :artist artist :rating rating :is-ripped is-ripped))



;;; Define global variable `*db*` that serves as database for CDs

(defvar *db* nil)



;;; Define function `add-cd` to add CD to database
;;;
;;; Parameters
;;; ----------
;;; cd : plist

(defun add-cd (cd) (push cd *db*))



;;; Define function `dump-db` to print database to `*standard-output*` in human-readable format
;;;   (If desired, you can implement this in one line by looping over `*db*`.)
;;;
;;; Parameters
;;; ----------
;;; None

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~11t~a~%~}~%" cd)))



;;; Define function `prompt-read` to prompt user for single piece of info
;;;
;;; Parameters
;;; ----------
;;; prompt : str

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))



;;; Define function `prompt-for-cd` using `prompt-read` (with appropriate prompts and wrapper
;;; functions) and `make-cd`
;;;
;;; Parameters
;;; ----------
;;; None

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Is ripped [y/n]: ")))



;;; Define function `add-cds` that
;;; 1.  prompts user to enter CDs until she says no and
;;; 2.  adds each CD she enters to the database `*db*`
;;;
;;; Parameters
;;; ----------
;;; None

(defun add-cds ()
  (loop (add-cd (prompt-for-cd))
    (if (not (y-or-n-p "Add another [y/n]: ")) (return))))



;;; Define function `save-db` to save database to file in Lisp-readable format
;;;
;;; Parameters
;;; ----------
;;; filename : str

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))



;;; Define function `load-db` to load database
;;;
;;; Parameters
;;; ----------
;;; filename : str

(defun load-db (filename)
  (with-open-file (in filename)         ; Default is `:direction :input`
    (with-standard-io-syntax (setf *db* (read in)))))



;;; Define function `select-by-artist` that returns all CDs by a given artist
;;; Note: We will deprecate this function after we generalize it below.
;;;
;;; Parameters
;;; ----------
;;; artist : str

(defun select-by-artist (artist)
  (remove-if-not
    #'(lambda (cd) (equal (getf cd :artist) artist))
    *db*))



;;; Define function `select` that returns all CDs in `*db*` matching conditions given by a selector
;;; function
;;;
;;; Parameters
;;; ----------
;;; selector-fn : function

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))



;;; Define function `where` that serves as general selector-function generator, using a keyword
;;; parameter list
;;; Note: We will deprecate this function after optimizing it below.
;;;
;;; Parameters
;;; ----------
;;; :title : str
;;; :artist : str
;;; :rating : int [0, 10]
;;; :is-ripped : bool
;;;     Include a supplied-p parameter for this parameter
;;;
;;; Output
;;; ------
;;; Anonymous selector function

(defun where (&key title artist rating (is-ripped nil is-ripped-p))
  #'(lambda (cd)
    (and
      (if title       (equal (getf cd :title)     title)     t)
      (if artist      (equal (getf cd :artist)    artist)    t)
      (if rating      (equal (getf cd :rating)    rating)    t)
      (if is-ripped-p (equal (getf cd :is-ripped) is-ripped) t))))



;;; Define function `update` (!!!)
;;; Note: We will deprecate this function after optimizing it below.

(defun update (selector-fn &key title artist rating (is-ripped nil is-ripped-p))
  (setf *db*
    (mapcar
      #'(lambda (row)
        (when (funcall selector-fn row)
          (if title       (setf (getf row :title)     title))
          (if artist      (setf (getf row :artist)    artist))
          (if rating      (setf (getf row :rating)    rating))
          (if is-ripped-p (setf (getf row :is-ripped) is-ripped)))
        row) *db*)))



;;; Define function `delete-rows` to delete rows from the database (!!!)

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))



;;; (!!!) Stopped at "Removing Duplication and Winning Big" : 2024-06-23