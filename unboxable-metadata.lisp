(in-package :unboxables)

(defvar *unboxable-info-table* (make-hash-table))
(declaim (inline unboxable-info))
(defun unboxable-info (name &optional (error-if-not-exists t))
  (multiple-value-bind (info existsp)
      (gethash name *unboxable-info-table*)
    (cond (existsp
           info)
          (error-if-not-exists
           (error "No unboxable-struct with name ~S" name))
          (t
           nil))))
(defun (setf unboxable-info) (info name)
  (if info
      (setf (gethash name *unboxable-info-table*) info)
      (remhash name *unboxable-info-table*)))

(defstruct (unboxable-primitive-info (:conc-name unboxable-primitive-))
  name
  fields
  total-size)

(defstruct (unboxable-field-info (:conc-name unboxable-field-))
  name
  offset
  type
  ctype
  size
  accessor
  initform)

;; (define-unboxable single-float :size 4)
;; (define-unboxable double-float :size 8)

(defun type-size (type)
  (switch (type :test (lambda (t1 t2)
                        (ignore-errors (type= t1 t2))))
    ('(signed-byte 08) 1)
    ('(signed-byte 16) 2)
    ('(signed-byte 32) 4)
    ('(signed-byte 64) 8)
    ('(unsigned-byte 08) 1)
    ('(unsigned-byte 16) 2)
    ('(unsigned-byte 32) 4)
    ('(unsigned-byte 64) 8)
    ('single-float 4)
    ('double-float 8)
    (t
     (cond ((null type)
            (error "Cannot find size of TYPE ~S" type))
           ((listp type)
            (let* ((element-type (first type))
                   (dimensions   (rest type))
                   (total-size (reduce #'* dimensions :initial-value 1)))
              (* (type-size element-type) total-size)))
           (t
            (let ((info (unboxable-info type)))
              (unboxable-primitive-total-size info)))))))

(defun type-dimensions (type)
  (if (ignore-errors (subtypep type 'number))
      ()
      (if (listp type)
          (rest type) ; dimensions
          1)))

(defun type-ctype (type)
  (switch (type :test (lambda (t1 t2)
                        (ignore-errors (type= t1 t2))))
    ('(signed-byte 08) :uint8)
    ('(signed-byte 16) :uint16)
    ('(signed-byte 32) :uint32)
    ('(signed-byte 64) :uint64)
    ('(unsigned-byte 08) :int8)
    ('(unsigned-byte 16) :int16)
    ('(unsigned-byte 32) :int32)
    ('(unsigned-byte 64) :int64)
    ('single-float :float)
    ('double-float :double)
    (t :pointer)))

(declaim (ftype (function ((or symbol cons))
                          (values (or symbol cons)
                                  positive-fixnum
                                  list
                                  positive-fixnum
                                  positive-fixnum))
                parse-unboxable-spec))
(defun parse-unboxable-spec (unboxable-spec)
  "Returns five values:
  - ELEMENT-TYPE
  - ELEMENT-SIZE
  - ARRAY-DIMENSIONS
  - ARRAY-SIZE
  - TOTAL-SIZE
"
  (if (listp unboxable-spec)
      (let* ((element-type     (first unboxable-spec))
             (array-dimensions (rest unboxable-spec))
             (elt-total-size   (nth-value 4 (parse-unboxable-spec element-type)))
             (array-size       (reduce #'* array-dimensions :initial-value 1)))
        (values element-type
                elt-total-size
                array-dimensions
                array-size
                (* elt-total-size array-size)))
      (values unboxable-spec
              (type-size unboxable-spec)
              ()
              1
              (type-size unboxable-spec))))
