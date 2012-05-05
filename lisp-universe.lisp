;;;; lisp-universe.lisp

(in-package #:lisp-universe)

(defmacro volume (radius)
  `(* 4/3 pi (expt ,radius 3)))

(rt:deftest test-volume (volume 3) 113.09733552923254d0)

(defmacro square (n)
  `(* ,n ,n))


(defclass 2d-vector ()
  ((x :type float :initarg :x :accessor vx)
   (y :type float :initarg :y :accessor vy)))

(defmacro coords (v)
  `(list (vx ,v) (vy ,v)))

(defmacro 2dv (&optional (x 0) (y 0))
  `(make-instance '2d-vector :x ,x :y ,y))

(defmacro 2dv- (vect1 vect2)
  "Returns a new vector by subtraction."
  `(make-instance '2d-vector
    :x (- (vx ,vect1) (vx ,vect2))
    :y (- (vy ,vect1) (vy ,vect2))))

(rt:deftest test-2dv-
  (coords (2dv- (2dv 15 30) (2dv 5 -2)))
  (10 32))

(defmacro 2dv/ (v n)
  "Returns a new vector by division."
  `(2dv (/ (vx ,v) ,n) (/ (vy ,v) ,n)))

(defmacro 2dv-mag (v)
  `(sqrt (+ (square (vx ,v)) (square (vy ,v)))))

(rt:deftest test-2dv-mag
  (2dv-mag (2dv 3 10))
  10.440307)

(defun 2dv-norm (v)
  (let ((mag (2dv-mag v)))
    (if (> mag 0)
      (2dv/ v mag))))

(rt:deftest test-2dv-norm
  (coords (2dv-norm (2dv 3 10)))
  (0.28734788 0.95782626))

(defmacro 2dv-mult (v n)
  "Returns a new vector by multiplication."
  `(2dv (* (vx ,v) ,n) (* (vy ,v) ,n)))

(rt:deftest test-2dv-mult
  (coords (2dv-mult (2dv 3 10) 5))
  (15 50))

(defun 2dv-dist (v1 v2)
  (let ((sv (2dv- v1 v2)))
    (sqrt (+ (square (vx sv)) (square (vy sv))))))

(rt:deftest test-2dv-dist
  (2dv-dist (2dv 3 10) (2dv -8 207))
  197.30687)










