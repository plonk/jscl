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

(defun pathname (namestring))

(defun merge-pathnames (a b))

(defun pathname-match-p (pathname wildcard))

(defgeneric pathnamep (value))
(defmethod pathnamep ((v pathname))
  t)
(defmethod pathnamep (v)
  nil)

(defclass logical-pathname (pathname))

(defun logical-pathname (pathspec))
(defun translate-logical-pathname (pathname))

(defun logical-pathname-translations (host))
;; and (setf logical-pathname-translations)

(defun parse-namestring (thing))
