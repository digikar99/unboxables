(in-package :unboxables)

(declaim (inline %make-unboxable))
(defstruct (unboxable (:constructor %make-unboxable))
  element-type
  element-ctype
  (pointer         nil :type foreign-pointer)
  (element-size     0  :type (unsigned-byte 32))
  (array-size       1  :type (unsigned-byte 32))
  (array-dimensions () :type list)
  (total-size       0  :type (unsigned-byte 32)))

(defmethod print-object ((o unboxable) stream)
  (print-unreadable-object (o stream :type t)
    (with-slots (element-type array-dimensions total-size pointer) o
      (format stream "~S~%    at ~S (~D bytes)"
              (cons element-type array-dimensions)
              pointer
              total-size))))


(defun make-unboxable (unboxable-spec)
  (declare (optimize speed))
  (multiple-value-bind (element-type element-size array-dimensions array-size total-size)
      (parse-unboxable-spec unboxable-spec)
    (let* ((ptr (foreign-alloc :uint8 :count (* element-size array-size)))
           (ub  (%make-unboxable :pointer ptr
                                 :element-type element-type
                                 :element-ctype (type-ctype element-type)
                                 :element-size element-size
                                 :total-size total-size
                                 :array-size array-size
                                 :array-dimensions array-dimensions)))
      (tg:finalize ub (lambda () (foreign-free ptr)))
      ub)))

(defun generate-constructor (name fields field-infos size)
  (with-gensyms (struct struct-ptr )
    `((declaim (inline ,(symbolicate 'make '- name)))
      (defun ,(symbolicate 'make '- name)
          (&optional ,@(loop :for (name default . rest) :in fields
                             :collect `(,name ,default)))
        (declare (optimize speed))
        (let* ((,struct-ptr  (foreign-alloc :uint8 :count ,size))
               (,struct
                 (%make-unboxable :pointer ,struct-ptr
                                  :element-type ',name
                                  :element-ctype ',(type-ctype name)
                                  :element-size ,size
                                  :total-size ,size)))
          (tg:finalize ,struct (lambda ()
                                 (foreign-free ,struct-ptr)))
          ,@(loop :for field :in field-infos
                  :collect (with-slots ((field-name name) offset ctype size)
                               field
                             (if (eq :pointer ctype)
                                 `(foreign-funcall "memcpy"
                                                   :pointer
                                                   (inc-pointer ,struct-ptr ,offset)
                                                   :pointer
                                                   (unboxable-pointer ,field-name)
                                                   :int
                                                   ,size)
                                 `(setf (mem-ref ,struct-ptr ',ctype ,offset)
                                        ,field-name))))
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
             `(mem-ref (unboxable-pointer object)
                       ',ctype ,offset)
             (multiple-value-bind
                   (element-type element-size array-dimensions array-size total-size)
                 (parse-unboxable-spec type)
               `(%make-unboxable :pointer (inc-pointer (unboxable-pointer object) ,offset)
                                 :element-type ',element-type
                                 :element-ctype ',(type-ctype element-type)
                                 :element-size ,element-size
                                 :array-size ,array-size
                                 :array-dimensions ',array-dimensions
                                 :total-size ,total-size))))

      (defun ,(symbolicate accessor '*) (object-pointer)
        (declare (type foreign-pointer object-pointer))
        ,(if (not (eq ctype :pointer))
             `(mem-ref object-pointer ',ctype ,offset)
             `(inc-pointer object-pointer ,offset)))

      (defun (setf ,accessor) (value object)
        (declare (type ,name object))
        ,(if (not (eq ctype :pointer))
             `(setf (mem-ref (unboxable-pointer object) ',ctype ,offset)
                    value)
             `(foreign-funcall "memcpy"
                               :pointer
                               (inc-pointer (unboxable-pointer object) ,offset)
                               :pointer
                               (unboxable-pointer value)
                               :int
                               ,size))
        value)

      (defun (setf ,(symbolicate accessor '*)) (value object-pointer)
        (declare (type foreign-pointer object-pointer))
        ,(if (not (eq ctype :pointer))
             `(setf (mem-ref object-pointer ',ctype ,offset)
                    value)
             `(foreign-funcall "memcpy"
                               :pointer
                               (inc-pointer object-pointer ,offset)
                               :pointer
                               (unboxable-pointer value)
                               :int
                               ,size))
        value))))

(defmacro define-unboxable-primitive (name &body fields)
  "
NAME can also be
  (NAME &KEY INITIAL-CONTENTS)
with INITIAL-CONTENTS being a single dimensional list.

Each of FIELDS should be of the form
  (FIELD-NAME DEFAULT &KEY TYPE ACCESSOR)
"

  (let* ((total-offset-so-far 0)
         (field-infos
           (loop :for field :in fields
                 :collect
                 (destructuring-bind (field-name
                                      &optional default
                                      &key type
                                        (accessor (symbolicate name '- field-name)))
                     field
                   (declare (ignorable default))
                   (let ((size  (type-size type))
                         (ctype (type-ctype type)))
                     (incf total-offset-so-far size)
                     (make-unboxable-field-info :name field-name
                                                :type type
                                                :ctype ctype
                                                :offset (- total-offset-so-far size)
                                                :size size
                                                :accessor accessor)))))
         (field-codes
           (loop :for field :in field-infos
                 :collect (with-slots (name type offset accessor ctype size) field
                            `(make-unboxable-field-info :name ',name
                                                        :type ',type
                                                        :ctype ',ctype
                                                        :offset ,offset
                                                        :size ,size
                                                        :accessor ',accessor)))))

    (with-gensyms (struct-info)

      `(progn
         (defcstruct ,name
           ,@(loop :for field :in field-infos
                   :collect (with-slots ((field-name name) ctype) field
                              `(,field-name ,ctype))))
         (let ((,struct-info (make-unboxable-primitive-info
                              :name ',name
                              :total-size ,total-offset-so-far
                              :fields (list ,@field-codes))))
           (setf (unboxable-info ',name) ,struct-info))

         (declaim (inline ,(symbolicate name '- 'p)))
         (defun ,(symbolicate name '- 'p) (object)
           (declare (optimize speed))
           (and (unboxable-p object)
                (eq ',name (unboxable-element-type object))
                (null (unboxable-array-dimensions object))))
         (deftype ,name ()
           '(and unboxable (satisfies ,(symbolicate name '- 'p))))
         ,@(generate-constructor name fields field-infos total-offset-so-far)
         ,@(loop :for field :in field-infos
                 :nconcing (generate-accessor name field))
         ',name))))
