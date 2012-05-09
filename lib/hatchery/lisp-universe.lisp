;;;; lisp-universe.lisp

(in-package #:lisp-universe)




(defmacro volume (radius)
  `(* 4/3 pi (expt ,radius 3)))

(rt:deftest test-volume (volume 3) 113.09733552923254d0)

(defmacro square (n)
  `(* ,n ,n))

(defmacro round-to (number precision)
  (let ((div (expt 10 precision)))
    `(float (/ (round (* ,number ,div)) ,div))))

(rt:deftest test-round-to (round-to 1.23456789 2) 1.23)


(defclass 2d-vector ()
  ((x :type float :initarg :x :accessor vx)
   (y :type float :initarg :y :accessor vy)))

(defmacro coords (v)
  `(list (vx ,v) (vy ,v)))

(defmacro 2dv (&optional (x 0) (y 0))
  `(make-instance '2d-vector :x ,x :y ,y))

(defmacro 2dv- (vect1 vect2)
  "Returns a new vector by subtraction."
  `(2dv
    (- (vx ,vect1) (vx ,vect2))
    (- (vy ,vect1) (vy ,vect2))))

(rt:deftest test-2dv-
  (coords (2dv- (2dv 15 30) (2dv 5 -2)))
  (10 32))

(defmacro 2dv+ (vect1 vect2)
  "Returns a new vector by addition."
  `(2dv
    (+ (vx ,vect1) (vx ,vect2))
    (+ (vy ,vect1) (vy ,vect2))))

(rt:deftest test-2dv+
  (coords (2dv+ (2dv 15 30) (2dv 5 -2)))
  (20 28))

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
      (2dv/ v mag)
      v)))

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




    ; particleCount : 300,
    ; starCount     : 4,
    ; gravitation   : 6.673e-11,
    ; maxRadius     : 3,
    ; maxDensity    : 5




(defparameter *gravitation* 6.67e-11)
(defparameter *max-radius* 4.0)
(defparameter *max-density* 30.0)

(defclass universe ()
  ((particles :type list :accessor particles :initform ())
   (width :initarg :width :type fixnum :reader uw :initform 0)
   (height :initarg :height :type fixnum :reader uh :initform 0)
   (particle-count :initarg :particle-count :type fixnum :reader particle-count :initform 0)))

(defclass particle ()
  ((position :initarg :position :accessor p-pos)
   (velocity :initarg :velocity :accessor velocity)
   (radius :type float :initarg :radius :reader radius)
   (mass :type float :initarg :mass :reader p-mass)))

(defun new-particle (universe)
  (setq radius (random *max-radius*))
  (make-instance 'particle
    :position (2dv (random (uw universe)) (random (uh universe)))
    :velocity (2dv)
    :radius (round-to radius 1)
    :mass (* radius *max-density*)))


(defun attraction-force (p1 p2)
  (let ((dist (2dv-dist (p-pos p1) (p-pos p2))))
    (if (> dist 0)
      (/ (* *gravitation* (p-mass p1) (p-mass p2)) (square dist))
      0)))

(rt:deftest test-attraction-force
  (let
    ((p1 (make-instance 'particle :position (2dv 0 0) :mass 40))
     (p2 (make-instance 'particle :position (2dv 0 100) :mass 60)))
    (attraction-force p1 p2))
  1.6008e-11)

(defun attraction-vector (p1 p2)
  "Calculates the attraction angle between two particles, and multiplies the vector by the gravitational force."
  (let ((angle (2dv- (p-pos p1) (p-pos p2))) (force (attraction-force p1 p2)))
    (print force)
    (2dv-mult (2dv-norm angle) force)))

(defun adjust-particle-velocity (p1 p2)
  "Adjust particle (p1) velocity towards particle (p2)."
  (setf (velocity p1) (2dv- (velocity p1) (attraction-vector p1 p2)))
  ())

(defun update-particle-position (particle universe)
  (loop for other-particle in (particles universe) do
    (unless (equal particle other-particle)
      (adjust-particle-velocity particle other-particle)
      (setf (p-pos particle) (2dv+ (p-pos particle) (velocity particle))))))

(defun get-frame-data (particle)
  `((:p . ,(coords (p-pos particle)))
    (:r . ,(radius particle))))




(defun new-universe (width height particle-count)
  "Set the *universe* global variable and spawn particles."
  (setq univ (make-instance 'universe :width width :height height :particle-count particle-count))

  (loop for i from 1 to particle-count do
    (push (new-particle univ) (particles univ)))
  univ)

(rt:deftest test-new-universe (= (list-length (particles (new-universe))) *particle-count*) t)



(defun build-frame-data (frames width height particle-count)
  "Builds a 2D array of particle frame data."
  (setq data (make-list particle-count))
  (setq universe (new-universe width height particle-count))
  (setq particles (particles universe))
  (loop for f from 0 to (- frames 1) do
    (loop for p from 0 to (- particle-count 1) do
      (setq particle (nth p particles))
      (update-particle-position particle universe)

      (push (get-frame-data particle) (nth p data))))
  data)


; Main entry for executable
(defun main (args)
  (let 
    ((frames (read-from-string (nth 1 args)))
     (width (read-from-string (nth 2 args)))
     (height (read-from-string (nth 3 args)))
     (particle-count (read-from-string (nth 4 args))))
    (write-string (json:encode-json-to-string (build-frame-data frames width height particle-count)))))

; (defun json-frame-data (frames)
;   (json:encode-json (build-frame-data frames)))








