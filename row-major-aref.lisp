(in-package #:unboxables)

(declaim (inline unboxable-row-major-aref
                 (setf unboxable-row-major-aref)))
(defun unboxable-row-major-aref (unboxable index)
  (declare (type unboxable unboxable)
           (type (unsigned-byte 32) index))
  (let* ((ub unboxable)
         (ptr      (unboxable-pointer ub))
         (elt-type (unboxable-element-type ub))
         (elt-size (unboxable-element-size ub)))
    (if (and (atom elt-type)
             (not (unboxable-info elt-type)))
        (mem-ref ptr (type-ctype elt-type) (* index elt-size))
        (multiple-value-bind
              (element-type element-size array-dimensions array-size total-size)
            (parse-unboxable-spec elt-type)
          (%make-unboxable :pointer (inc-pointer ptr (* index elt-size))
                           :element-type element-type
                           :element-size element-size
                           :total-size total-size
                           :array-size array-size
                           :array-dimensions array-dimensions)))))

(defun (setf unboxable-row-major-aref) (value unboxable index)
  (declare (type unboxable unboxable)
           (type (unsigned-byte 32) index))
  (let* ((ub unboxable)
         (ptr       (unboxable-pointer ub))
         (elt-type  (unboxable-element-type ub))
         (elt-ctype (unboxable-element-ctype ub))
         (elt-size  (unboxable-element-size ub)))
    (if (and (atom elt-type)
             (not (unboxable-info elt-type)))
        (setf (mem-ref ptr elt-ctype (* index elt-size)) value)
        (setf (mem-ref ptr elt-ctype (* index elt-size))
              (mem-ref (unboxable-pointer value) elt-ctype)))
    value))



(declaim (inline unboxable-row-major-aref*
                 (setf unboxable-row-major-aref*)))

(defun unboxable-row-major-aref* (unboxable index)
  (declare (type unboxable unboxable)
           (type (unsigned-byte 32) index))
  (let* ((ub unboxable)
         (ptr      (unboxable-pointer ub))
         (elt-size (unboxable-element-size ub)))
    (inc-pointer ptr (* index elt-size))))

;; TODO: Optimize this.
(defun (setf unboxable-row-major-aref*) (value unboxable index)
  (declare (type unboxable unboxable)
           (type (unsigned-byte 32) index))
  (let* ((ub unboxable)
         (ptr       (unboxable-pointer ub))
         (elt-size  (unboxable-element-size ub))
         (elt-type  (unboxable-element-ctype ub))
         (elt-ctype (unboxable-element-ctype ub)))
    (if (and (atom elt-ctype)
             (not (unboxable-info elt-type)))
        (setf (mem-ref ptr elt-ctype (* index elt-size))
              value)
        (setf (mem-ref ptr elt-ctype (* index elt-size))
              (mem-ref value elt-ctype)))
    value))
