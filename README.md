# unboxables

A simple wrapper around CFFI to enable contiguously allocated arrays of structures in Common Lisp.

## Basic Example

```lisp
CL-USER> (ql:quickload "unboxables")
To load "unboxables":
  Load 1 ASDF system:
    unboxables
; Loading "unboxables"
[package unboxables]...
("unboxables")
CL-USER> (defpackage #:unboxables-demo
           (:use :cl :unboxables))
#<PACKAGE "UNBOXABLES-DEMO">
EXCL> (in-package #:unboxables-demo)
#<PACKAGE "UNBOXABLES-DEMO">
```

First, we define a `point` as an unboxable primitive of two `single-float`:

```lisp
UNBOXABLES-DEMO> (define-unboxable-primitive point
                   (x 0.0f0 :type single-float)
                   (y 0.0f0 :type single-float))
POINT
```

This defines a `point` type, a `make-point` constructor as well as the relevant accessor functions.

```lisp
UNBOXABLES-DEMO> (make-point)
#<UNBOXABLE (POINT)
    at #.(SB-SYS:INT-SAP #X7FAAD0000B20) (8 bytes)>
UNBOXABLES-DEMO> (make-point)
#<UNBOXABLE (POINT)
    at #.(SB-SYS:INT-SAP #X7FAAD00039D0) (8 bytes)>
UNBOXABLES-DEMO> (make-point 2.0 3.0)
#<UNBOXABLE (POINT)
    at #.(SB-SYS:INT-SAP #X7FAAD00039F0) (8 bytes)>
UNBOXABLES-DEMO> (point-x (make-point 2.0 3.0))
2.0
UNBOXABLES-DEMO> (point-y (make-point 2.0 3.0))
3.0
```

However, after defining the `point` type above using `define-primitive-type`, we can also define a packed array of points. For example, the following defines a 10x10 packed array of `point`:

```lisp
UNBOXABLES-DEMO> (make-unboxable '(point 10 10))
#<UNBOXABLE (POINT 10 10)
    at #.(SB-SYS:INT-SAP #X7FAAD0000F00) (800 bytes)>
```

Individual `point`s in the array can be accessed using `unboxable-row-major-aref`:

```lisp
UNBOXABLES-DEMO> (unboxable-row-major-aref (make-unboxable '(point 10 10)) 2)
#<UNBOXABLE (POINT)
    at #.(SB-SYS:INT-SAP #X7FAAD0001240) (8 bytes)>
UNBOXABLES-DEMO> (let ((parray (make-unboxable '(point 10 10))))
                   (setf (unboxable-row-major-aref parray 1)
                         (make-point 2.0f0 3.0f0))
                   (list (point-x (unboxable-row-major-aref parray 1))
                         (point-y (unboxable-row-major-aref parray 1))))
(2.0 3.0)
```

## Unboxing

The above simple example deals with boxed values, creating a `unboxable` wrapper on every array access. This can be avoided through the use of alternative accessors that end in `*`. These accessors operate on pointers. The following should illustrate the difference in terms of how they are used:

```lisp
UNBOXABLES-DEMO> (make-point 1.0 2.0)
#<UNBOXABLE (POINT)
    at #.(SB-SYS:INT-SAP #X7FAAD0003A30) (8 bytes)>
UNBOXABLES-DEMO> (point-x *)
1.0
UNBOXABLES-DEMO> (unboxable-pointer (make-point 1.0 2.0))
#.(SB-SYS:INT-SAP #X7FAAD00039F0)
UNBOXABLES-DEMO> (point-x* *)
1.0
```

The real difference only becomes significant at scale - and given the efficiency of SBCL, it may not even be that significant.

Contrast the following code written in usual Common Lisp:

```lisp
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
```

with the following written with the help of unboxables:

```lisp
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
      (cffi:incf-pointer ptr size))
    x-total))
```

In terms of performance, because of the packed nature of unboxables, the `point-add-all-x` can be faster than `spoint-add-all-x`:

```lisp
UNBOXABLES-DEMO> (let ((a (make-spoint-array '(1000 1000))))
                   (time (loop repeat 100 do (spoint-add-all-x a))))
Evaluation took:
  0.336 seconds of real time
  0.337198 seconds of total run time (0.337198 user, 0.000000 system)
  100.30% CPU
  744,522,780 processor cycles
  0 bytes consed
NIL
UNBOXABLES-DEMO> (let ((a (make-point-array '(1000 1000))))
                   (time (loop repeat 100 do (point-add-all-x a))))
Evaluation took:
  0.183 seconds of real time
  0.184546 seconds of total run time (0.184546 user, 0.000000 system)
  101.09% CPU
  407,471,880 processor cycles
  0 bytes consed
NIL
```

Indeed for smaller arrays, the difference is rather insignificant:

```lisp
UNBOXABLES-DEMO> (let ((a (make-spoint-array '(100 100))))
                   (time (loop repeat 10000 do (spoint-add-all-x a))))
Evaluation took:
  0.219 seconds of real time
  0.223600 seconds of total run time (0.223600 user, 0.000000 system)
  102.28% CPU
  493,700,648 processor cycles
  0 bytes consed
NIL
UNBOXABLES-DEMO> (let ((a (make-point-array '(100 100))))
                   (time (loop repeat 10000 do (point-add-all-x a))))
Evaluation took:
  0.187 seconds of real time
  0.184300 seconds of total run time (0.184300 user, 0.000000 system)
  98.40% CPU
  406,934,716 processor cycles
  0 bytes consed
NIL
```

## Nested structures

Perhaps the best part is that this allows structures to be nested without overhead:

```lisp
UNBOXABLES-DEMO> (define-unboxable-primitive triplet
                   (a (make-point) :type point)
                   (b (make-point) :type point)
                   (c (make-point) :type point))
TRIPLET
UNBOXABLES-DEMO> (make-unboxable '(triplet 100 100))
#<UNBOXABLE (TRIPLET 100 100)
    at #.(SB-SYS:INT-SAP #X7FAADA164010) (240000 bytes)>
```

Also

```lisp
UNBOXABLES-DEMO> (define-unboxable-primitive point-and-array
                   (a (make-point) :type point)
                   (b (make-unboxable '(point 10)) :type (point 10)))
POINT-AND-ARRAY
```

## Issues

Same elements of an array need not be EQ to each other.

```lisp
UNBOXABLES-DEMO> (let ((parray (make-unboxable '(point 10 10))))
                   (eq (unboxable-row-major-aref parray 1)
                       (unboxable-row-major-aref parray 1)))
NIL
```

In addition, because we are often dealing with raw pointers, one may run into segmentation faults requiring lisp image restarts.

## Limitations and Future Plans

- more robust type checking: will add
- `unboxable-aref` in addition to row-major-aref: will add
- inheritance: perhaps, won't add

## trivial-garbage:finalize

While we use CFFI and `foreign-alloc` and `foreign-free`, we perform the free-ing through `trivial-garbage:finalize` aka `tg:finalize`. Thus, no memory leaks should occur as long as this works correctly.
