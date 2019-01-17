;;; arrays.lisp

;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(/debug "loading array.lisp!")

(defun upgraded-array-element-type (typespec &optional environment)
  (declare (ignore environment))
  (if (eq typespec 'character)
      'character
      t))

(defun check-dimensions (contents dimensions)
  (when dimensions
    (unless (or (vectorp contents) (listp contents))
       (error "not a sequence"))
    (unless (= (length contents) (car dimensions))
      (error "dimension mismatch"))

    (cond
      ((vectorp contents)
       (dotimes (i (length contents))
         (check-dimensions (aref contents i) (cdr dimensions))))
      ((listp contents)
       (dolist (elt contents)
         (check-dimensions elt (cdr dimensions)))))))

(defun assign-contents (array start contents dimensions)
  (if dimensions
      (cond
        ((vectorp contents)
         (dotimes (n (car dimensions))
           (setf start
                 (assign-contents array start (aref contents n) (cdr dimensions))))
         start)
        ((listp contents)
         (dolist (elt contents)
           (setf start
                 (assign-contents array start elt (cdr dimensions))))
         start)
        (t (error "ASSIGN-CONTENTS: Not a sequence")))
      (progn
        (storage-vector-set array start contents)
        (1+ start))))

(defun make-array (dimensions &key element-type initial-element initial-contents adjustable fill-pointer)
  (let* ((dimensions (ensure-list dimensions))
         (size (!reduce #'* dimensions 1))
         (array (make-storage-vector size)))
    ;; Upgrade type
    (if (eq element-type 'character)
        (progn
          (oset 1 array "stringp")
          (setf element-type 'character
                initial-element (or initial-element #\space)))
        (setf element-type t))

    (when (and (listp dimensions)
               (not (null (cdr dimensions)))
               fill-pointer)
      (error "FILL-POINTER cannot be specified on multidimensional arrays."))

    ;; Initialize array
    (if initial-contents
        (progn
          (check-dimensions initial-contents dimensions)
          (assign-contents array 0 initial-contents dimensions))
        (storage-vector-fill array initial-element))
    ;; Record and return the object
    (setf (oget array "type") element-type)
    (setf (oget array "dimensions") dimensions)
    (setf (oget array "fillpointer") fill-pointer)
    array))

(defun arrayp (x)
  (storage-vector-p x))

(defun adjustable-array-p (array)
  (unless (arrayp array)
    (error "~S is not an array." array))
  t)

(defun array-element-type (array)
  (unless (arrayp array)
    (error "~S is not an array." array))
  (if (eq (oget array "stringp") 1)
      'character
      (oget array "type")))

(defun array-dimensions (array)
  (unless (arrayp array)
    (error "~S is not an array." array))

  (if ((oget array "hasOwnProperty") "dimensions")
      (oget array "dimensions")
      (list (storage-vector-size array))))

;; TODO: Error checking
(defun array-dimension (array axis)
  (nth axis (array-dimensions array)))

(defun array-row-major-index (array &rest subscripts)
  (unless (arrayp array)
    (error "~S is not an array." array))
  (unless (apply #'array-in-bounds-p array subscripts)
    (error "Out of bounds"))

  (apply #'+ (maplist #'(lambda (x y)
                          (* (car x) (apply #'* (cdr y))))
                      subscripts
                      (array-dimensions array))))

(defun array-rank (array)
  (length (array-dimensions array)))

(defun array-total-size (array)
  (unless (arrayp array)
    (error "~S is not an array." array))

  (storage-vector-size array))

(defun array-in-bounds-p (array &rest subscripts)
  (unless (arrayp array)
    (error "~S is not an array." array))

  (if (= 1 (array-rank array))
      (progn
        (unless (= 1 (length subscripts))
          (error "number of subscripts not 1"))
        (and (integerp (car subscripts))
             (not (minusp (car subscripts)))
             (< (car subscripts) (storage-vector-size array))))

      (progn
        (unless (= (length subscripts) (array-rank array))
          (error "Got ~S subscripts for rank ~S array"
                 (length subscripts) (array-rank array)))
        (every (lambda (i n)
                 (and (integerp i) (not (minusp i)) (< i n)))
               #'< subscripts (array-dimensions array)))))

(defun aref (array &rest indices)
  (unless (arrayp array)
    (error "~S is not an array." array))

  (storage-vector-ref array (apply #'array-row-major-index array indices)))

(defun aset (array &rest rest)
  (unless (arrayp array)
    (error "~S is not an array." array))

  (storage-vector-set array
                      (apply #'array-row-major-index array (butlast rest))
                      (car (last rest))))

(define-setf-expander aref (array &rest indices)
  (let ((g!array (gensym))
        (g!indices (gensym))
        (g!value (gensym)))
    (values (list g!array g!indices)
            (list array (cons 'list indices))
            (list g!value)
            `(aset ,array ,@indices ,g!value)
            `(aref ,array ,@indices))))

(defun array-has-fill-pointer-p (array)
  (and (oget array "fillpointer") t))

(defun fill-pointer (array)
  (unless (arrayp array)
    (error "~S is not an array" array))
  (unless (array-has-fill-pointer-p array)
    (error "~S does not have a fill pointer" array))
  (oget array "fillpointer"))

(defun set-fill-pointer (array new-value)
  (unless (arrayp array)
    (error "~S is not an array" array))
  (unless (array-has-fill-pointer-p array)
    (error "~S does not have a fill pointer" array))
  (setf (oget array "fillpointer") new-value))

(defsetf fill-pointer set-fill-pointer)


;;; Vectors

(defun vectorp (x)
  (and (arrayp x) (null (cdr (array-dimensions x)))))

(defun vector (&rest objects)
  (list-to-vector objects))

(defun vector-push-extend (new-element vector)
  (unless (vectorp vector)
    (error "~S is not a vector." vector))
  ;; Note that JS will automatically grow the array as new elements
  ;; are assigned, so no need to do `adjust-array` here.
  (storage-vector-set! vector (fill-pointer vector) new-element)
  (incf (fill-pointer vector)))
