(defpackage :unboxables
  (:use :cl :cffi :alexandria)
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
