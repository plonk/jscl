(/debug "loading pathname.lisp!")

(defclass pathname ()
  ((host      :reader pathname-host      :initarg :host)
   (device    :reader pathname-device    :initarg :device)
   (name      :reader pathname-name      :initarg :name)
   (directory :reader pathname-directory :initarg :directory)
   (type      :reader pathname-type      :initarg :type)
   (version   :reader pathname-version   :initarg :version)))

(defun make-pathname (&rest args)
  (apply #'make-instance 'pathname args))

;; -------------------------
(defun pathname (namestring)
  (parse-tokens (namestring-tokenize namestring)))

(defun namestring-tokenize (namestring &key (start 0))
  (let ((spos (position #\/ namestring :start start)))
    (cond
      ((= start (length namestring)) nil)
      ((eql spos start) (cons "/" (namestring-tokenize namestring :start (1+ spos))))
      (spos (cons (subseq namestring start spos)
                  (namestring-tokenize namestring :start spos)))
      (t (list (subseq namestring start))))))
;; (trace namestring-tokenize)

(defun dir-basename (tokens)
  (let (type dir basename)
    (if (string= (car tokens) "/")
        (setq type :absolute)
        (setq type :relative))
    (if (string= (car (last tokens)) "/")
        (progn
          (setq tokens (remove "/" tokens :test #'string=))
          (list (cons type tokens) nil))
        (progn
          (setq tokens (remove "/" tokens :test #'string=))
          (list (cons type (butlast tokens)) (car (last tokens)))))))

(defun massage-dir (dir)
  (let ((dir1 (substitute :up ".." dir :test #'string=)))
    (if (equal '(:relative) dir1)
        nil
        dir1)))

(defun name-type (basename)
  (let ((i (position #\. basename :from-end t)))
    (cond
      ((null i) (list basename nil))
      ((= i 0) (list basename nil)) ; dot file
      (t (list (subseq basename 0 i)
               (subseq basename (1+ i)))))))

(defun parse-tokens (tokens)
  (destructuring-bind (dir basename)
      (dir-basename tokens)
    (destructuring-bind (name type)
        (name-type basename)
      (make-pathname :directory (massage-dir dir) :name name :type type))))
;; -------------------------

(defun merge-pathnames (a b))

(defun pathname-match-p (pathname wildcard))

(defgeneric pathnamep (value))
(defmethod pathnamep ((v pathname))
  t)
(defmethod pathnamep (v)
  nil)

(defclass logical-pathname (pathname)
  ())

(defun logical-pathname (pathspec))
(defun translate-logical-pathname (pathname))

(defvar *logical-pathname-tranlsations* nil)

(defun logical-pathname-translations (host)
  (let ((entry (assoc host *logical-pathname-tranlsations* :test #'string-equal)))
    (if entry
        (cdr entry)
        (error "logical host ~S not defined" host))))

(defun logical-pathname-translations-set (host translations)
  (let ((entry (assoc host *logical-pathname-tranlsations* :test #'string-equal)))
    (setf (cdr entry) translations))
  (push (cons host translations) *logical-pathname-tranlsations*))

(define-setf-expander logical-pathname-translations (host)
  (let ((g!value (gensym)))
    (values '()
            '()
            (list g!value)
            `(logical-pathname-translations-set ,host ,g!value)
            `(logical-pathname-translations ,host))))

(defun parse-namestring (thing))
