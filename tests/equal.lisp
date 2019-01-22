(test (equal '(1 2) '(1 2)))
(test (equal 1 1))
(test (equal "abc" "abc"))
(test (not (equal "abc" "def")))
(test (not (equal "Abc" "abc")))


;; EQUALP
(test (not (equalp 'a 'b)))
(test (equalp 'a 'a))
(test (equalp 3 3))
(test (equalp 3 3.0))
(test (equalp 3.0 3.0))
;; (test (equalp #c(3 -4) #c(3 -4)))
;; (test (equalp #c(3 -4.0) #c(3 -4)))
(test (not (equalp (cons 'a 'b) (cons 'a 'c))))
(test (equalp (cons 'a 'b) (cons 'a 'b)))
(test (equalp #\A #\A))
(test (equalp #\A #\a))
(test (equalp "Foo" "Foo"))
(test (equalp "Foo" (copy-seq "Foo")))
(test (equalp "FOO" "foo"))

(let ((array1 (make-array 6 :element-type 'integer
                          :initial-contents '(1 1 1 3 5 7)))
      (array2 (make-array 8 :element-type 'integer
                          :initial-contents '(1 1 1 3 5 7 2 6)
                          :fill-pointer 6)))
  (test (equalp array1 array2))
  (let ((vector1 (vector 1 1 1 3 5 7)))
    (test (equalp array1 vector1))))
