(in-package :unboxables)

(defstruct spoint
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float))

(defun make-spoint-array (dimensions)
  (let* ((total-size (reduce #'* dimensions :initial-value 1))
         (a (make-array dimensions :element-type 'spoint)))
    (dotimes (idx total-size)
      (setf (row-major-aref a idx) (make-spoint)))
    a))

(defun spoint-add-all-x (point-array)
  (declare (type (simple-array spoint 2) point-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (array-size (array-total-size point-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (incf x-total (spoint-x (row-major-aref point-array idx))))
    x-total))

(define-unboxable-primitive point
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float))

(defun make-point-array (dimensions)
  (make-unboxable (cons 'point dimensions)))

(defun point-add-all-x (point-array)
  (declare (type unboxable point-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (ptr  (unboxable-row-major-aref* point-array 0))
        (size (unboxable-element-size point-array))
        (array-size (unboxable-array-size point-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (incf x-total (point-x* ptr))
      (incf-pointer ptr size))
    x-total))

(defun point-add-all-x (point-array)
  (declare (type unboxable point-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (array-size (unboxable-array-size point-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (incf x-total (point-x* (unboxable-row-major-aref* point-array idx))))
    x-total))

(defstruct striplet
  (a (make-spoint) :type spoint)
  (b (make-spoint) :type spoint)
  (c (make-spoint) :type spoint))

(defun make-striplet-array (dimensions)
  (let* ((total-size (reduce #'* dimensions :initial-value 1))
         (a (make-array dimensions :element-type 'spoint)))
    (dotimes (idx total-size)
      (setf (row-major-aref a idx) (make-striplet)))
    a))

(defun striplet-add-all-x (point-array)
  (declare (type (simple-array striplet 2) point-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (array-size (array-total-size point-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (let ((s (row-major-aref point-array idx)))
        (incf x-total (spoint-x (striplet-a s)))
        (incf x-total (spoint-x (striplet-b s)))
        (incf x-total (spoint-x (striplet-c s)))))
    x-total))

(define-unboxable-primitive triplet
  (a (make-point) :type point)
  (b (make-point) :type point)
  (c (make-point) :type point))

(define-unboxable-primitive point-and-array
  (a (make-point) :type point)
  (b (make-unboxable '(point 10)) :type (point 10)))

(defun make-triplet-array (dimensions)
  (make-unboxable (cons 'triplet dimensions)))

(defun triplet-add-all-x (triplet-array)
  (declare (type unboxable triplet-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (array-size (unboxable-array-size triplet-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (let ((s (unboxable-row-major-aref triplet-array idx)))
        (incf x-total (point-x (triplet-a s)))
        (incf x-total (point-x (triplet-b s)))
        (incf x-total (point-x (triplet-c s)))))
    x-total))

(defun triplet-add-all-x (triplet-array)
  (declare (type unboxable triplet-array)
           (optimize speed))
  (let ((x-total 0.0f0)
        (ptr (unboxable-pointer triplet-array))
        (elt-size (unboxable-element-size triplet-array))
        (array-size (unboxable-array-size triplet-array)))
    (declare (type single-float x-total))
    (dotimes (idx array-size)
      (incf x-total (point-x* (triplet-a* ptr)))
      (incf x-total (point-x* (triplet-b* ptr)))
      (incf x-total (point-x* (triplet-c* ptr)))
      (incf-pointer ptr elt-size))
    x-total))

(define-unboxable-primitive pair
  (a 0.0f0 :type single-float)
  (b 0.0f0 :type single-float))

(defstruct spair
  (a 0.0f0 :type single-float)
  (b 0.0f0 :type single-float))

;; (pair-a o) ;=> (mem-ref o-ptr a-type :offset a-offset)

(defun access-pair (pair)
  (declare (optimize speed))
  (pair-b* pair))

(defun access-spair (pair)
  (declare (optimize speed))
  (spair-b pair))

