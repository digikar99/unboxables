(in-package #:unboxables)

(defmacro do-unboxable ((&rest field-vars)
                        (unboxable element-type &optional unboxable-ptr)
                        &body body)

  (check-type element-type symbol)

  (let* ((info (unboxable-info element-type))
         (field-infos (unboxable-primitive-fields info))
         (unboxable-ptr (or unboxable-ptr (gensym "UNBOXABLE-PTR"))))
    (flet ((accessor (var)
             (symbolicate
              (unboxable-field-accessor
               (find var field-infos :key #'unboxable-field-name))
              '*)))

      (with-gensyms (element-size end-ptr total-size)

        (once-only (unboxable)

          `(let* ((,element-size  (unboxable-element-size ,unboxable))
                  (,total-size    (unboxable-total-size ,unboxable))
                  (,unboxable-ptr (unboxable-pointer ,unboxable))
                  (,end-ptr       (cffi:inc-pointer ,unboxable-ptr ,total-size)))

             (assert (eq ',element-type (unboxable-element-type ,unboxable)))

             (do ((,unboxable-ptr ,unboxable-ptr
                                  (cffi:inc-pointer ,unboxable-ptr ,element-size)))
                 ((cffi:pointer-eq ,unboxable-ptr ,end-ptr))

               (symbol-macrolet
                   (,@(loop :for var :in field-vars
                            :collect `(,var (,(accessor var) ,unboxable-ptr))))
                 ,@body))))))))

#|
(let ((parray (make-unboxable '(point 10)))
      (i -1))
  (do-unboxable (x y) (parray point)
    (setf x (incf i))
    (setf y (incf i)))
  (do-unboxable (x y) (parray point)
    (print x)
    (print y)))
|#
