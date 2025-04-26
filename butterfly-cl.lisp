
(eval-when (:compile-toplevel)
  (ql:quickload :zpng))

(defpackage butterfly
  (:use :cl ))

(in-package :butterfly)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defparameter *width* 2000
  "width of the picture, in pixels")
(defparameter *height* 1100
  "height of the picture, in pixels")
(declaim (type fixnum *width* *height*))

(defmacro exp-exp (x)
  "Expand into (exp (- (exp X))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (> ,x 85.0d0)
      0.0d0
      (exp (- (exp ,x)))))

(defmacro exp-exp+exp (x y)
  "Expand into (exp (- (+ (exp X) (exp Y))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (or (> ,x 85.0d0) (> ,y 85.0d0))
      0.0d0
      (exp (- (+ (exp ,x) (exp ,y))))))

(declaim (ftype (function (double-float double-float) double-float) C))

(defun C (x y)
  "Return C(x,y)."
  (declare (type double-float x y))
  (expt
   (sin (+ (* 14 (atan (/ (* 100 (+ y
                                    (/ x 4.0d0)
                                    (- (/ 1.0d0 25.0d0))))
                          (+ (abs (- (* 100.0d0 x)
                                     (* 25.0d0 y)
                                     (* 3.0d0 (atan (- (* 100.0d0 x)
                                                       (* 25.0d0 y))))))
                             1.0d0))))
           (* 14 (abs (- (/ x 2.0d0)
                         (/ y 8.0d0))))))
   4))

(declaim (ftype (function (fixnum double-float double-float) double-float) K))

(defun K (v x y)
  "Return K(v,x,y)."
  (declare (type fixnum v)
           (type double-float x y))
  (let ((sum1 0.0d0))
    (declare (type double-float sum1))
    (loop for s of-type fixnum from 1 to 60
          do (let ((exp0
                    (- (* 25
                          (- (* (expt
                                 (sin
                                  (+ (sin (* 2.0d0 s))
                                     (* (+ 6 (sin (* 1.0d0 s s)))
                                        (+ (* (sin (* 7.0d0 s)) (/ x 2.0d0))
                                           (* (cos (* 7.0d0 s)) (/ (- y 8) 2.0d0))))))
                                 10)
                                (expt
                                 (sin
                                  (+ (sin (* 3.0d0 s))
                                     (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                        (- (* (sin (* 7.0d0 s)) (/ (- y 8) 2.0d0))
                                           (* (cos (* 7.0d0 s)) (/ x 2.0d0))))))
                                 10.0d0))
                             0.1d0)))))
               (declare (type double-float exp0))
               (incf sum1
                     (* (/ 5.0d0 2.0d0)
                        (+ (/ 2 25.0d0)
                           (* (/ 3 50.0d0)
                              (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                        (/ (+ (sin (* 5.0d0 s))
                              (sin (* 2.0d0 s))
                              3.0d0)
                           5.0d0)
                        (exp-exp exp0))))) ; loop
    sum1) ; let
  )

(declaim (ftype (function (fixnum double-float double-float double-float) double-float) A))

(defun A (v x y Cxy)
  "Return A(v,x,y)."
  (declare (type fixnum v))
  (declare (type double-float x y Cxy))
  (exp
   (+
    (- (exp
        ;; first exponential:
        (* 200.0d0
           (+ y
              (/ x 4.0d0)
              (/ v 50.0d0) 
              (- (* .25d0
                    (abs
                     (sin
                      (* (/ 12.0d0 5.0d0)
                         (+ (* .7d0 (abs (- x (/ y 4.0d0))))
                            (* .3d0 (sqrt (abs (- x (/ y 4.0d0)))))))))))))))
    (- (exp
        ;; second exponential:
        (- (* 200.0d0
              (+ y
                 (/ x 4.0d0)
                 (/ 7.0d0 20.0d0)
                 (- (/ (* v 7.0d0) 50.0d0))
                 (* 0.2d0 (atan (* 6.0d0 (abs (- x (/ y 4.0d0))))))
                 (* 0.2d0 (atan (* 40.0d0 (abs (- x (/ y 4.0d0))))))
                 (- (* (/ 23.0d0 20.0d0)
                       (+ 1.5d0
                          (* (/ 1.0d0 25.0d0)
                             (cos
                              (* 10.0d0
                                 (+ y
                                    (/ x 4.0d0)
                                    (/ 6.0d0 25.0d0)))))
                          (* 0.03d0 Cxy)
                          (* 0.3d0
                             (atan (* 30.0d0
                                      (+ y (/ x 4.0d0) (- 0.25d0))))))
                       (abs (- x (/ y 4.0d0)))))))))))))

(declaim (ftype (function (double-float double-float) double-float) E))

(defun E (x y)
  "Return E(x,y)."
  (declare (type double-float x y))
  (let* ((exp1a
           (- (* 100.0d0 (expt (+ (* 3 y) (* 0.75d0 x) 0.27d0) 4))))
         (exp1b
           (- (* 100.0d0 (expt
                          (abs
                           (* 7
                              (+ 1.0d0
                                 (/ 1.0d0
                                    (+ (sqrt
                                        (the (double-float 0.0d0)
                                             (abs
                                              (+ (* 100 y)
                                                 (* 25 x)
                                                 (- 6)))))
                                       0.3d0)))
                              (- x (/ y 4.0d0))))
                          (+ (* 3 y) (* 0.75d0 x) 2.27d0)))))
         (exp1 (+ exp1a exp1b 10.0d0))
         (exp2 (+ (* 200
                     (abs
                      (+ y
                         (/ x 4.0d0)
                         (- 0.2d0)
                         (* 3 (- x (/ y 4.0d0)) (- x (/ y 4.0d0))))))
                  -32.0d0))
         (exp3 (+ (* 500
                     (abs
                      (+ y
                         (/ x 4.0d0)
                         (- (/ 1.0d0 20.0d0))
                         (- (* 0.7d0 (sqrt (abs (- x (/ y 4.0d0)))))))))
                  -2.5d0)))
    (declare (type double-float exp1a exp1b exp2 exp3))
    (- 1.0d0
       (* (exp-exp exp1)
          (- 1.0d0
             (exp-exp+exp exp2 exp3))))))

(declaim (ftype (function (double-float double-float) double-float) L))

(defun L (x y)
  "Return L(x,y)."
  (declare (type double-float x y))
  (let ((sum1 0.0d0))
    (declare (type double-float sum1))
    (loop for s of-type fixnum from 1 to 25
          do
             (incf sum1
                   (expt
                    (sin (+ (* (+ 80 (* 30 (sin (* 1.0d0 s s))))
                               (atan (/ (- (+ (* 100.0d0 y) (* 25.0d0 x))
                                           (* 4.0d0 (sin (* 1.0d0 s))))
                                        (+ (abs (- (* 100.0d0 x)
                                                   (* 25.0d0 y)
                                                   (* 3.0d0 (atan (- (* 100.0d0 x)
                                                                     (* 25.0d0 y))))))
                                           1.0d0))))
                            (abs (- (/ x 2.0d0) (/ y 8.0d0)))
                            (* 4.0d0 (sin (* 5.0d0 s)))))
                    6))) ; loop
    sum1))

(declaim (ftype (function (double-float double-float double-float) double-float) W))

(defun W (x y Cxy)
  "Return W(x,y)."
  (declare (type double-float x y Cxy))
  
  (let* ((omega1 (+ (- (* 40 Cxy))
                    (/ 196.0d0 5.0d0)
                    (* (/ 4.0d0 5.0d0)
                       (sqrt (the (double-float 0.0d0)
                                  (+ (* (- x (/ y 4.0d0))
                                        (- x (/ y 4.0d0)))
                                     (* (+ y (/ x 4.0d0))
                                        (+ y (/ x 4.0d0)))))))))
         (omega2 (- (* 40
                       (+ (* 5 (abs
                                (+ y
                                   (/ x 4.0d0)
                                   (- (/ 3 50.0d0))
                                   (* (/ 1.0d0 3.0d0)
                                      (- x (/ y 4.0d0))
                                      (- x (/ y 4.0d0))))))
                          (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                          (- (/ 2.0d0 5.0d0))))))
         (omega3 (+
                  (- (* 1000
                        (+ (abs (- x (/ y 4.0d0))))))
                  100.0d0
                  (- (* 90.0d0 (atan (+ (* 8 y)
                                        (* 2 x)
                                        (/ 8 5.0d0)))))))
         (omega4 (* 1000 (+ (abs (- x (/ y 4.0d0)))
                            (- (/ 7 50.0d0))
                            (* (/ 9.0d0 20.0d0)
                               (+ y (/ x 4.0d0) 0.2d0)))))
         (omega5 (* 70 (+ (abs
                           (+
                            (* 5
                               (abs
                                (+ y
                                   (/ x 4.0d0)
                                   (- (/ 3 50.0d0))
                                   (* (/ 1.0d0 3.0d0)
                                      (- x (/ y 4.0d0))
                                      (- x (/ y 4.0d0))))))
                            (expt (abs (- (* 2 x) (/ y 2.0d0))) 3)
                            (- (/ 2 5.0d0))))
                          (- (/ 1.0d0 200.0d0)))))
         (omega6 (+ (* 700
                       (abs
                        (+
                         (abs (- x (/ y 4.0d0)))
                         (- 0.1d0)
                         (* 0.09d0 (atan (* 8
                                            (+ y
                                               (/ x 4.0d0)
                                               (/ 1 5.0d0))))))))
                    (- (/ 21 20.0d0)))))

    (declare (type double-float omega1 omega2 omega3 omega4 omega5 omega6))
    
    (+ (* (- (exp-exp+exp omega1 omega2))
          (- 1 (exp-exp+exp omega3 omega4)))
       (- (exp-exp omega5))
       (- (exp-exp omega6))
       1.0d0)))

(declaim (ftype (function (fixnum double-float double-float) double-float) H))

(defun H (v x y)
  "Return H(v,x,y)."
  (declare (type fixnum v)
           (type double-float x y))

  (let* ((Cxy (C x y))
         (Exy (E x y))
         (A0xy (A 0 x y Cxy))
         (Lxy (L x y))
         (Kxy (K v x y))
         (Wxy (W x y Cxy))
         (exp2a (exp
                 (+ (* 2 y)
                    (* 0.5d0 x)
                    (/ 2.0d0 5.0d0)
                    (- (* 2.0d0 (abs (- x (/ y 4.0d0))))))))
         (exp2b (exp
                 (+ (* 8 y)
                    (* 2 x)
                    (/ 2.0d0 5.0d0)
                    (- (abs (- (* 8 x) (* 2 y)))))))
         (exp2 (+ exp2a exp2b))
         (exp3 (- (* 50.0d0
                     (- (* (expt (cos (+ (* 2 y) 
                                         (* x 0.5d0)
                                         (/ 7 5.0d0)
                                         (- (abs (- (* 2 x)
                                                    (* y 0.5d0))))))
                                 80)
                           (expt (sin (+ (* 20 y)
                                         (* 5 x)
                                         (abs (- (* 20 x) (* 5 y)))))
                                 2))
                        (expt (+ (* 2.7d0 y)
                                 (/ (* 27 x) 40.0d0)
                                 (/ 81.0d0 250.0d0)
                                 )
                              10)
                        (/ 49.0d0 50.0d0))))))
    (declare (type double-float Cxy Exy Lxy Wxy Kxy A0xy exp2a exp2b exp2 exp3))

    (+
     ;; first term:
     (* (/ (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) 20.0d0)
        (- 1.0d0 A0xy)
        (- 1.0d0 Exy)
        Kxy) 
     ;; second term:
     (* (/ (+ 2 (* 3.0d0 v)) 5.0d0)
        (* A0xy
           (A 1 x y Cxy)
           (- 1 Exy)
           (/ (+ 50.0d0 Lxy) 50.0d0)
           (exp-exp exp2)
           Wxy))
     ;; third term:
     (exp-exp exp3)
     ;; fourth term:
     (* 0.1d0 Exy (* (- v 1.0d0) (- v 1))))))

(declaim (ftype (function (double-float) fixnum) F))

(defun F (x)
  "Return F(x)."
  (declare (type double-float x))
  (let ((a (- (* 1000.0 x)))
        (b (* 1000.0 (- x 1.0))))
    (declare (type double-float a b))
    (floor
     (* 255.0
        (exp-exp a)
        (expt (abs x) (exp-exp b))))))


(defun draw-pic-from-rgb-arrays (height width r-array g-array b-array export-file)
  "Export picture to file from RGB arrays."
  (declare (type fixnum height width)
           (type (simple-array fixnum) r-array g-array b-array))
  (let* ((alpha 255)
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (declare (type fixnum alpha))
    (with-open-file (stream export-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((r (aref r-array (- n 1) (- m 1)))
                (g (aref g-array (- n 1) (- m 1)))
                (b (aref b-array (- n 1) (- m 1))))
            (declare (type fixnum r g b))
            (zpng:write-pixel (list r g b alpha) png))))
      (zpng:finish-png png))))

(defun draw-heatmap-from-values (height width value-array export-file)
  "Create png file with heatmap from value-array.
Algorithm to convert value to HSL then RGB is inspired by: https://stackoverflow.com/questions/17525215/calculate-color-values-from-green-to-red/30612603#30612603"
  (declare (type fixnum height width)
           (type (simple-array double-float) value-array))

  (labels ((hue-to-rgb (p q tt)
             (declare (type double-float p q tt))
             (when (< tt 0.0d0) (setq tt (+ tt 1.0d0)))
             (when (> tt 1.0d0) (setq tt (- tt 1.0d0)))
             (cond ((< tt (/ 1.0d0 6.0d0))
                    (+ p (* (- q p) 6.0d0 tt)))
                   ((< tt 0.5d0) q)
                   ((< tt (/ 2.0d0 3.0d0))
                    (+ p (* (- q p) (- (/ 2.0d0 3.0d0) tt) 6.0d0)))
                   (t p))))

    (declare (ftype (function (double-float double-float double-float) double-float) hue-to-rgb))
    
    (let* ((r-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (g-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (b-array (make-array (list height width) :element-type 'fixnum :initial-element 0))
           (value-min 0.0d0)
           (value-max 0.0d0)
           (value-range 0.0d0))
      (declare (type (simple-array fixnum) r-array g-array b-array)
               (type double-float value-min value-max value-range))

      ;; (1) calculate values min, max and range
      (setq value-min (aref value-array 0 0))
      (setq value-max (aref value-array 0 0))
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let ((z (aref value-array (- n 1) (- m 1))))
            (declare (type double-float z))
            (when (> z value-max) (setq value-max z))
            (when (< z value-min) (setq value-min z)))))
      (setq value-range (- value-max value-min))

      ;; (2) calculate RGB arrays
      (loop for n of-type fixnum from 1 to height do
        (loop for m of-type fixnum from 1 to width do
          (let* ((z (aref value-array (- n 1) (- m 1)))
                 (w (/ (- z value-min) value-range)) ; between 0 and 1
                 (h (/ (* w 1.2d0) 3.60d0)) ; hie between 0° and 120°/360°
                 (s 1.0d0) ; saturation
                 (l 0.5d0) ; lightness
                 (q (- (+ l s) (* l s)))
                 (p (- (* 2 l) q))
                 (r (floor (* 255 (hue-to-rgb p q (+ h (/ 1.0d0 3.0d0))))))
                 (g (floor (* 255 (hue-to-rgb p q h))))
                 (b (floor (* 255 (hue-to-rgb p q (- h (/ 1.0d0 3.0d0)))))))
            (declare (type double-float z w h s l p q)
                     (type fixnum r g b))
            (setf (aref r-array (- n 1) (- m 1)) r)
            (setf (aref g-array (- n 1) (- m 1)) g)
            (setf (aref b-array (- n 1) (- m 1)) b))))

      ;; (3) create heatmap picture
      (draw-pic-from-rgb-arrays height width r-array g-array b-array export-file))))

;;; 

(defun draw-all-heatmaps ()
  "Export all heatmaps of intermediary functions to PNG files."
  
  (let ((C-array (make-array (list *height* *width*) :element-type 'double-float :initial-element 0.0d0))
        (value-array (make-array (list *height* *width*) :element-type 'double-float :initial-element 0.0d0)))
    (declare (type (simple-array double-float) C-array value-array))

    ;; C

    (format t "C: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let ((x (/ (- m 1000.0d0) 960.0d0))
              (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref C-array (- n 1) (- m 1)) 
                (C x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* C-array "C-heatmap.png")
    (format t "done.~%")

    ;; K0

    ;; (format t "K0: calculating... ")
    ;; (loop for n of-type fixnum from 1 to *height* do
    ;;   (loop for m of-type fixnum from 1 to *width* do
    ;;     (let ((x (/ (- m 1000.0d0) 960.0d0))
    ;;           (y (/ (- 451.0d0 n) 960.0d0)))
    ;;       (declare (type double-float x y))
    ;;       (setf (aref value-array (- n 1) (- m 1)) 
    ;;             (K 0 x y)))))
    ;; (format t "drawing... ")
    ;; (draw-heatmap-from-values *height* *width* value-array "K0-heatmap.png")
    ;; (format t "done.~%")
    
    ;; K1
    
    ;; (format t "K1: calculating... ")
    ;; (loop for n of-type fixnum from 1 to *height* do
    ;;   (loop for m of-type fixnum from 1 to *width* do
    ;;     (let ((x (/ (- m 1000.0d0) 960.0d0))
    ;;           (y (/ (- 451.0d0 n) 960.0d0)))
    ;;       (declare (type double-float x y))
    ;;       (setf (aref value-array (- n 1) (- m 1)) 
    ;;             (K 1 x y)))))
    ;; (format t "drawing... ")
    ;; (draw-heatmap-from-values *height* *width* value-array "K1-heatmap.png")
    ;; (format t "done.~%")
    
    ;; K2
    
    ;; (format t "K2: calculating... ")
    ;; (loop for n of-type fixnum from 1 to *height* do
    ;;   (loop for m of-type fixnum from 1 to *width* do
    ;;     (let ((x (/ (- m 1000.0d0) 960.0d0))
    ;;           (y (/ (- 451.0d0 n) 960.0d0)))
    ;;       (declare (type double-float x y))
    ;;       (setf (aref value-array (- n 1) (- m 1)) 
    ;;             (K 2 x y)))))
    ;; (format t "drawing... ")
    ;; (draw-heatmap-from-values *height* *width* value-array "K2-heatmap.png")
    ;; (format t "done.~%")

    
    (format t "A0: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0))
               (Cxy (aref C-array (- n 1) (- m 1))))
          (declare (type double-float x y Cxy))
          (setf (aref value-array (- n 1) (- m 1)) 
                (A 0 x y Cxy)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "A0-heatmap.png")
    (format t "done.~%")

    ;; A1
    
    (format t "A1: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0))
               (Cxy (aref C-array (- n 1) (- m 1))))
          (declare (type double-float x y Cxy))
          (setf (aref value-array (- n 1) (- m 1)) 
                (A 1 x y Cxy)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "A1-heatmap.png")
    (format t "done.~%")

    ;; E

    (format t "E: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let ((x (/ (- m 1000.0d0) 960.0d0))
              (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref value-array (- n 1) (- m 1)) 
                (E x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "E-heatmap.png")
    (format t "done.~%")

    ;; L

    (format t "L: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let ((x (/ (- m 1000.0d0) 960.0d0))
              (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref value-array (- n 1) (- m 1)) 
                (L x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* C-array "L-heatmap.png")
    (format t "done.~%")
    
    ;; W

    (format t "W: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0))
               (Cxy (aref C-array (- n 1) (- m 1))))
          (declare (type double-float x y Cxy))
          (setf (aref value-array (- n 1) (- m 1)) 
                (W x y Cxy)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "W-heatmap.png")
    (format t "done.~%")

    ;; H0
    
    (format t "H0: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref value-array (- n 1) (- m 1)) 
                (H 0 x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "H0-heatmap.png")
    (format t "done.~%")
    
    ;; H1
    
    (format t "H1: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref value-array (- n 1) (- m 1)) 
                (H 1 x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "H1-heatmap.png")
    (format t "done.~%")

    ;; H2
    
    (format t "H2: calculating... ")
    (loop for n of-type fixnum from 1 to *height* do
      (loop for m of-type fixnum from 1 to *width* do
        (let* ((x (/ (- m 1000.0d0) 960.0d0))
               (y (/ (- 451.0d0 n) 960.0d0)))
          (declare (type double-float x y))
          (setf (aref value-array (- n 1) (- m 1)) 
                (H 2 x y)))))
    (format t "drawing... ")
    (draw-heatmap-from-values *height* *width* value-array "H2-heatmap.png")
    (format t "done.~%")))


(defun save-bmp-from-rgb-arrays (filename r-array g-array b-array width height)
  "Save the RGB data from the 2D arrays R-ARRAY, G-ARRAY, B-ARRAY to a BMP file."
  (let* ((row-bytes (* 3 width))  ; 3 bytes per pixel (RGB)
         (padding (mod (- 4 (mod row-bytes 4)) 4))  ; Padding to align rows to 4 bytes
         (row-size (+ row-bytes padding))  ; Total row size with padding
         (pixel-data-size (* row-size height))  ; Total size for the pixel data
         (file-size (+ 14 40 pixel-data-size)))  ; Total file size (header + data)
    (with-open-file (out filename :direction :output :if-exists :supersede
                        :element-type '(unsigned-byte 8))
      ;; === BMP FILE HEADER ===
      (write-byte #x42 out)  ; 'B'
      (write-byte #x4D out)  ; 'M'
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) file-size) out))  ; File size
      (loop repeat 4 do (write-byte 0 out))  ; Reserved
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) 54) out))  ; Pixel data offset (54 bytes)

      ;; === DIB HEADER (BITMAPINFOHEADER) ===
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) 40) out))  ; Header size
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) width) out))  ; Width
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) height) out))  ; Height
      (write-byte 1 out)  ; Planes (1)
      (write-byte 0 out)  ; Reserved
      (write-byte 24 out)  ; Bits per pixel (24 bits: RGB)
      (write-byte 0 out)  ; Compression (0: none)
      (loop repeat 4 do (write-byte 0 out))  ; More zeroes (unused fields)
      (loop for shift in '(0 8 16 24)
            do (write-byte (ldb (byte 8 shift) pixel-data-size) out))  ; Image size
      (loop repeat 16 do (write-byte 0 out))  ; Unused fields in DIB header

      ;; === PIXEL DATA ===
      ;; BMP format stores pixels in bottom-up order (reverse row order)
      (loop for y downfrom (1- height) to 0 do
           (loop for x from 0 below width do
                (let ((r (aref r-array y x))  ; Accessing 2D arrays at (y, x)
                      (g (aref g-array y x))
                      (b (aref b-array y x)))
                  ;; BMP format uses BGR order (not RGB)
                  (write-byte b out)
                  (write-byte g out)
                  (write-byte r out)))
           ;; Padding to ensure row size is a multiple of 4 bytes
           (loop repeat padding do (write-byte 0 out))))))



(defun main ()
  "Main function. Create butterfly.png"
  (format t "1) Create RGB arrays...~%")
  (let ((export-file (merge-pathnames "butterfly-cl.bmp"))
        (r-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
        (g-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
        (b-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0)))
    (declare (type (simple-array fixnum) r-array g-array b-array))

    (format t "2) Calculate RGB components...~%")
    (loop for n of-type fixnum from 1 to *height* do
          (loop for m of-type fixnum from 1 to *width* do
                (progn
                  (let* ((x (/ (- m 1000.0d0) 960.0d0))
                         (y (/ (- 451.0d0 n) 960.0d0))
                         (r (F (H 0 x y)))
                         (g (F (H 1 x y)))
                         (b (F (H 2 x y))))
                    (declare (type double-float x y)
                             (type fixnum r g b))
                    (setf (aref r-array (- n 1) (- m 1)) r)
                    (setf (aref g-array (- n 1) (- m 1)) g)
                    (setf (aref b-array (- n 1) (- m 1)) b))
                  ;; print some information:
                  (when (and (= 0 (mod m 100)) (or (= 1 n) (= 0 (mod n 100))))
                    (let ((r (aref r-array (- n 1) (- m 1)))
                          (g (aref g-array (- n 1) (- m 1)))
                          (b (aref b-array (- n 1) (- m 1))))
                      (declare (type fixnum r g b))
                      (format t "n = ~a ; m = ~a ; RGB = ~a ~a ~a~%" n m r g b))))))
    
    (format t "3) Export pic...~%")
    ;; (draw-pic-from-rgb-arrays *height* *width* r-array g-array b-array export-file)
    (save-bmp-from-rgb-arrays export-file r-array g-array b-array *width* *height*)
    (format t "Done.~%")))

;;; (butterfly::main)

;;;; end
