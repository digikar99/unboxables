(defpackage :unboxables
  (:use :cl :alexandria)
  (:export #:make-unboxable
           #:define-unboxable-primitive

           #:unboxable
           #:unboxable-p
           #:unboxable-element-type
           #:unboxable-pointer
           #:unboxable-element-size
           #:unboxable-array-size
           #:unboxable-array-dimensions
           #:unboxable-total-size

           #:unboxable-row-major-aref
           #:unboxable-row-major-aref*))
