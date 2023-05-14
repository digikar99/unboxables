(defpackage :unboxables
  (:use :cl :alexandria)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-free
                #:foreign-pointer
                #:mem-ref
                #:inc-pointer
                #:incf-pointer)
  (:export #:make-unboxable
           #:define-unboxable-primitive

           #:unboxable
           #:unboxable-element-type
           #:unboxable-pointer
           #:unboxable-element-size
           #:unboxable-array-size
           #:unboxable-array-dimensions
           #:unboxable-total-size

           #:unboxable-row-major-aref
           #:unboxable-row-major-aref*))
