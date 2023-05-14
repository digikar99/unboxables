(in-package :unboxables)

(declaim (inline %make-unboxable))
(defstruct (unboxable (:constructor %make-unboxable))
  (element-type    nil :read-only t)
  (element-ctype   nil :read-only t)
  (pointer         nil :read-only t :type cffi:foreign-pointer)
  (element-size     0  :read-only t :type (unsigned-byte 32))
  (array-size       1  :read-only t :type (unsigned-byte 32))
  (array-dimensions () :read-only t :type list)
  (total-size       0  :read-only t :type (unsigned-byte 32)))

(defmethod print-object ((o unboxable) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (element-type array-dimensions total-size pointer) o
      (format stream "~S at #x~X (~D bytes)"
              (cons element-type array-dimensions)
              (cffi:pointer-address pointer)
              total-size))))


(defun make-unboxable (unboxable-spec)
  (multiple-value-bind (element-type element-size array-dimensions array-size total-size)
      (parse-unboxable-spec unboxable-spec)
    (let* ((ptr (cffi:foreign-alloc :uint8 :count (* element-size array-size)))
           (ub  (%make-unboxable :pointer ptr
                                 :element-type element-type
                                 :element-ctype (type-ctype element-type)
                                 :element-size element-size
                                 :total-size total-size
                                 :array-size array-size
                                 :array-dimensions array-dimensions)))
      (tg:finalize ub (lambda () (cffi:foreign-free ptr)))
      ub)))

(defun generate-constructor (name fields field-infos size)
  (with-gensyms (struct struct-ptr)
    `((declaim (inline ,(symbolicate 'make '- name)))
      (defun ,(symbolicate 'make '- name)
          (&optional ,@(loop :for (name default . rest) :in fields
                             :collect `(,name ,default)))
        (declare (ignorable ,@(mapcar #'first fields)))
        (let* ((,struct-ptr  (cffi:foreign-alloc :uint8 :count ,size))
               (,struct
                 (%make-unboxable :pointer ,struct-ptr
                                  :element-type ',name
                                  :element-ctype ',(type-ctype name)
                                  :element-size ,size
                                  :total-size ,size)))
          (tg:finalize ,struct (lambda ()
                                 (cffi:foreign-free ,struct-ptr)
                                 (setf (cffi:mem-ref ,struct-ptr :pointer)
                                       (cffi:null-pointer))))
          ,@(loop :for field :in field-infos
                  :collect
                  (with-slots ((field-name name) offset ctype size initform)
                      field
                    (cond ((null initform)
                           nil)
                          ((eq :pointer ctype)
                           `(cffi:foreign-funcall
                             "memcpy"
                             :pointer (cffi:inc-pointer ,struct-ptr ,offset)
                             :pointer (cond ((cffi:pointerp ,field-name)
                                             ,field-name)
                                            ((unboxable-p ,field-name)
                                             (unboxable-pointer ,field-name))
                                            (t
                                             ,field-name))
                             :int ,size))
                          (t
                           `(setf (cffi:mem-ref ,struct-ptr ',ctype ,offset)
                                  ,field-name)))))
          ,struct)))))



(defun generate-accessor (name field-info)

  (declare (type unboxable-field-info field-info))

  (with-slots ((field-name name) type ctype offset accessor size) field-info

    `((declaim (inline ,accessor (setf ,accessor)
                       ,(symbolicate accessor '*)
                       (setf ,(symbolicate accessor '*))))

      (defun ,accessor (object)
        (declare (type ,name object))
        ,(if (not (eq ctype :pointer))
             `(cffi:mem-ref (unboxable-pointer object)
                       ',ctype ,offset)
             (multiple-value-bind
                   (element-type element-size array-dimensions array-size total-size)
                 (parse-unboxable-spec type)
               `(%make-unboxable :pointer
                                 (cffi:inc-pointer (unboxable-pointer object) ,offset)
                                 :element-type ',element-type
                                 :element-ctype ',(type-ctype element-type)
                                 :element-size ,element-size
                                 :array-size ,array-size
                                 :array-dimensions ',array-dimensions
                                 :total-size ,total-size))))

      (defun ,(symbolicate accessor '*) (object-pointer)
        (declare (type cffi:foreign-pointer object-pointer))
        ,(if (not (eq ctype :pointer))
             `(cffi:mem-ref object-pointer ',ctype ,offset)
             `(cffi:inc-pointer object-pointer ,offset)))

      (defun (setf ,accessor) (value object)
        (declare (type ,name object))
        ,(if (not (eq ctype :pointer))
             `(setf (cffi:mem-ref (unboxable-pointer object) ',ctype ,offset)
                    value)
             `(cffi:foreign-funcall
               "memcpy"
               :pointer (cffi:inc-pointer (unboxable-pointer object) ,offset)
               :pointer (unboxable-pointer value)
               :int ,size))
        value)

      (defun (setf ,(symbolicate accessor '*)) (value object-pointer)
        (declare (type cffi:foreign-pointer object-pointer))
        ,(if (not (eq ctype :pointer))
             `(setf (cffi:mem-ref object-pointer ',ctype ,offset)
                    value)
             `(cffi:foreign-funcall
               "memcpy"
               :pointer (cffi:inc-pointer object-pointer ,offset)
               :pointer (unboxable-pointer value)
               :int
               ,size))
        value))))

(defmacro define-unboxable-primitive (name &body fields)
  "
NAME may also have CONC-NAME and PREDICATE like with CL:DEFSTRUCT

Each of FIELDS should be of the form
  (FIELD-NAME INITFORM &KEY TYPE ACCESSOR)

  INITFORM may be NIL in which case the slot will remain uninitialized.
"

  (multiple-value-bind (name conc-name predicate)

      (if (listp name)
          (values (first name)
                  (or (second (assoc :conc-name (rest name)))
                      (symbolicate (first name) '-))
                  (or (second (assoc :predicate (rest name)))
                      (symbolicate (first name) '- 'p)))
          (values name (symbolicate name '-)))

    (let* ((total-offset-so-far 0)
           (field-infos
             (loop :for field :in fields
                   :collect
                   (destructuring-bind (field-name
                                        initform
                                        &key type
                                          (accessor (symbolicate conc-name field-name)))
                       field
                     (let ((size  (type-size type))
                           (ctype (type-ctype type)))
                       (incf total-offset-so-far size)
                       (make-unboxable-field-info :name field-name
                                                  :type type
                                                  :ctype ctype
                                                  :offset (- total-offset-so-far size)
                                                  :size size
                                                  :accessor accessor
                                                  :initform initform)))))
           (field-codes
             (loop :for field :in field-infos
                   :collect (with-slots
                                  (name type offset accessor ctype size initform)
                                field
                              `(make-unboxable-field-info :name ',name
                                                          :type ',type
                                                          :ctype ',ctype
                                                          :offset ,offset
                                                          :size ,size
                                                          :accessor ',accessor
                                                          :initform ',initform)))))

      (with-gensyms (struct-info)

        `(progn
           (let ((,struct-info (make-unboxable-primitive-info
                                :name ',name
                                :total-size ,total-offset-so-far
                                :fields (list ,@field-codes))))
             (setf (unboxable-info ',name) ,struct-info))

           (declaim (inline ,predicate))
           (defun ,predicate (object)
             (and (unboxable-p object)
                  (eq ',name (unboxable-element-type object))
                  (null (unboxable-array-dimensions object))))
           (deftype ,name ()
             '(and unboxable (satisfies ,predicate)))
           ,@(generate-constructor name fields field-infos total-offset-so-far)
           ,@(loop :for field :in field-infos
                   :nconcing (generate-accessor name field))
           ',name)))))
