
(defpackage butterfly
  (:use :cl ))

(in-package :butterfly)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defparameter *width* 2000
  "width of the picture, in pixels")
(defparameter *height* 1100
  "height of the picture, in pixels")
(declaim (type fixnum *width* *height*))

(defparameter *one-over-two* 0.5d0)
(declaim (type double-float *one-over-two*))
(defparameter *one-over-three* 0.33333333333333333d0)
(declaim (type double-float *one-over-three*))
(defparameter *one-over-four* 0.25d0)
(declaim (type double-float *one-over-four*))
(defparameter *one-over-eight* 0.125d0)
(declaim (type double-float *one-over-eight*))
(defparameter *one-over-twenty* 0.05d0)
(declaim (type double-float *one-over-twenty*))
(defparameter *one-over-twenty-five* 0.04d0)
(declaim (type double-float *one-over-twenty-five*))
(defparameter *one-over-five* 0.2d0)
(declaim (type double-float *one-over-five* ))
(defparameter *one-over-forty* 0.025d0)
(declaim (type double-float *one-over-forty* ))
(defparameter *one-over-fifty* 0.02d0)
(declaim (type double-float *one-over-fifty* ))
(defparameter *one-over-two-hundred* 0.005d0)
(declaim (type double-float *one-over-two-hundred* ))
(defparameter *two-over-five* 0.4d0)
(declaim (type double-float *two-over-five*))
(defparameter *two-over-twenty-five* 0.08d0)
(declaim (type double-float *two-over-twenty-five*))
(defparameter *three-over-fifty* 0.06d0)
(declaim (type double-float *three-over-fifty*))
(defparameter *four-over-five* 0.8d0)
(declaim (type double-float *four-over-five*))
(defparameter *five-over-two* 2.5d0)
(declaim (type double-float *five-over-two*))
(defparameter *six-over-twenty-five* 0.24d0)
(declaim (type double-float *six-over-twenty-five* ))
(defparameter *seven-over-five* 1.4d0)
(declaim (type double-float *seven-over-five*))
(defparameter *seven-over-twenty* 0.35d0)
(declaim (type double-float *seven-over-twenty* ))
(defparameter *seven-over-fifty* 0.14d0)
(declaim (type double-float *seven-over-fifty* ))
(defparameter *eight-over-five* 1.6d0)
(declaim (type double-float *eight-over-five* ))
(defparameter *nine-over-twenty* 0.45d0)
(declaim (type double-float *nine-over-twenty* ))
(defparameter *twelve-over-five* 2.4d0)
(declaim (type double-float *twelve-over-five* ))
(defparameter *twenty-one-over-twenty* 1.05d0)
(declaim (type double-float *twenty-one-over-twenty* ))
(defparameter *twenty-three-over-twenty* 1.15d0)
(declaim (type double-float *twenty-three-over-twenty* ))
(defparameter *forty-nine-over-fifty* 0.98d0)
(declaim (type double-float *forty-nine-over-fifty* ))
(defparameter *eighty-one-over-two-hundred-fifty* 0.324d0)
(declaim (type double-float *eighty-one-over-two-hundred-fifty* ))
(defparameter *one-hundred-ninety-six-over-five* 39.2d0)
(declaim (type double-float *one-hundred-ninety-six-over-five* ))

(defmacro exp-exp (x)
  "Expand into (exp (- (exp X))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (> ,x 85.0d0)
       0.0d0
       (exp (- (exp ,x)))))

(defmacro exp-exp-exp (x y)
  "Expand into (exp (- (+ (exp X) (exp Y))), or 0 if X or Y is above 85 (in order to avoid overflow)."
  `(if (or (> ,x 85.0d0) (> ,y 85.0d0))
       0.0d0
       (exp (- (+ (exp ,x) (exp ,y))))))

(defun save-bmp-from-rgb-arrays (filename r-array g-array b-array width height)
  "Save the RGB data from the 2D arrays R-ARRAY, G-ARRAY, B-ARRAY to a BMP file."
  (locally
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let* ((row-bytes (* 3 width))    ; 3 bytes per pixel (RGB)
             (padding (mod (- 4 (mod row-bytes 4)) 4)) ; Padding to align rows to 4 bytes
             (row-size (+ row-bytes padding)) ; Total row size with padding
             (pixel-data-size (* row-size height)) ; Total size for the pixel data
             (file-size (+ 14 40 pixel-data-size))) ; Total file size (header + data)
        (with-open-file (out filename :direction :output :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
          ;; === BMP FILE HEADER ===
          (write-byte #x42 out)         ; 'B'
          (write-byte #x4D out)         ; 'M'
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) file-size) out)) ; File size
          (loop repeat 4 do (write-byte 0 out)) ; Reserved
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) 54) out)) ; Pixel data offset (54 bytes)

          ;; === DIB HEADER (BITMAPINFOHEADER) ===
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) 40) out)) ; Header size
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) width) out)) ; Width
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) height) out)) ; Height
          (write-byte 1 out)           ; Planes (1)
          (write-byte 0 out)           ; Reserved
          (write-byte 24 out)          ; Bits per pixel (24 bits: RGB)
          (write-byte 0 out)           ; Compression (0: none)
          (loop repeat 4 do (write-byte 0 out)) ; More zeroes (unused fields)
          (loop for shift in '(0 8 16 24)
                do (write-byte (ldb (byte 8 shift) pixel-data-size) out)) ; Image size
          (loop repeat 16 do (write-byte 0 out)) ; Unused fields in DIB header

          ;; === PIXEL DATA ===
          ;; BMP format stores pixels in bottom-up order (reverse row order)
          (loop for y downfrom (1- height) to 0 do
            (loop for x from 0 below width do
              (let ((r (aref r-array y x)) ; Accessing 2D arrays at (y, x)
                    (g (aref g-array y x))
                    (b (aref b-array y x)))
                ;; BMP format uses BGR order (not RGB)
                (write-byte b out)
                (write-byte g out)
                (write-byte r out)))
            ;; Padding to ensure row size is a multiple of 4 bytes
            (loop repeat padding do (write-byte 0 out)))))))



(declaim (ftype (function ((simple-array double-float (1)) double-float double-float) null) C))
(defun C (res x y)
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y))
  (let ((res1 (expt
               (sin (+ (* 14 (atan (/ (* 100 (+ y
                                                (* x *one-over-four*)
                                                (- *one-over-twenty-five*)))
                                      (+ (abs (- (* 100.0d0 x)
                                                 (* 25.0d0 y)
                                                 (* 3.0d0 (atan (- (* 100.0d0 x) (* 25.0d0 y))))))
                                         1.0d0))))
                       (* 14 (abs (- (* x *one-over-two*)
                                     (* y *one-over-eight*))))))
               4)))
    (declare (type double-float res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float) null) E))
(defun E (res x y)
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y))
  (let ((res1 (- 1.0d0
                 (* (exp-exp (+ 10.0d0
                                (- (* 100.0d0 (expt (+ (* 3 y) (* 0.75d0 x) 0.27d0) 4)))
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
                                                   (- x (* y *one-over-four*))))
                                               (+ (* 3 y) (* 0.75d0 x) 2.27d0))))))
                    (- 1.0d0
                       (exp-exp-exp
                        (- (* 200
                              (abs
                               (+ y
                                  (* x *one-over-four*)
                                  (- 0.2d0)
                                  (* 3 (- x (* y *one-over-four*)) (- x (* y *one-over-four*))))))
                           32.0d0)
                        (- (* 500
                              (abs
                               (+ y
                                  (* x *one-over-four*)
                                  (- *one-over-twenty*)
                                  (- (* 0.7d0 (sqrt (abs (- x (* y *one-over-four*)))))))))
                           2.5d0)))))))
    (declare (type double-float res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float) null) L))
(defun L (res x y)
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y))
  (let ((res1 0.0d0))
    (declare (type double-float res1))
    (loop for s of-type fixnum from 1 to 25
          do
             (incf res1
                   (expt
                    (sin (+ (* (+ 80 (* 30 (sin (* 1.0d0 s s))))
                               (atan (/ (- (+ (* 100.0d0 y) (* 25.0d0 x))
                                           (* 4.0d0 (sin (* 1.0d0 s))))
                                        (+ (abs (- (* 100.0d0 x)
                                                   (* 25.0d0 y)
                                                   (* 3.0d0 (atan (- (* 100.0d0 x)
                                                                     (* 25.0d0 y))))))
                                           1.0d0))))
                            (abs (- (* x *one-over-two*) (* y *one-over-eight*)))
                            (* 4.0d0 (sin (* 5.0d0 s)))))
                    6)))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float double-float) null) W))
(defun W (res x y Cxy)
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y Cxy))
  (let* ((omega1 (+ (- (* 40 Cxy))
                    *one-hundred-ninety-six-over-five*
                    (* *four-over-five*
                       (sqrt (the (double-float 0.0d0)
                                  (+ (* (- x (* y *one-over-four*))
                                        (- x (* y *one-over-four*)))
                                     (* (+ y (* x *one-over-four*))
                                        (+ y (* x *one-over-four*)))))))))
         (omega2 (- (* 40
                       (+ (* 5 (abs
                                (+ y
                                   (* x *one-over-four*)
                                   (- *three-over-fifty*)
                                   (* *one-over-three*
                                      (- x (* y *one-over-four*))
                                      (- x (* y *one-over-four*))))))
                          (expt (abs (- (* 2 x) (* y *one-over-two*))) 3)
                          (- *two-over-five*)))))
         (omega3 (+
                  (- (* 1000
                        (+ (abs (- x (* y *one-over-four*))))))
                  100.0d0
                  (- (* 90.0d0 (atan (+ (* 8 y)
                                        (* 2 x)
                                        *eight-over-five*))))))
         (omega4 (* 1000 (+ (abs (- x (* y *one-over-four*)))
                            (- *seven-over-fifty*)
                            (* *nine-over-twenty*
                               (+ y (* x *one-over-four*) 0.2d0)))))
         (omega5 (* 70 (+ (abs
                           (+
                            (* 5
                               (abs
                                (+ y
                                   (* x *one-over-four*)
                                   (- *three-over-fifty*)
                                   (* *one-over-three*
                                      (- x (* y *one-over-four*))
                                      (- x (* y *one-over-four*))))))
                            (expt (abs (- (* 2 x) (* y *one-over-two*))) 3)
                            (- *two-over-five*)))
                          (- *one-over-two-hundred*))))
         (omega6 (+ (* 700
                       (abs
                        (+
                         (abs (- x (* y *one-over-four*)))
                         (- 0.1d0)
                         (* 0.09d0 (atan (* 8
                                            (+ y
                                               (* x *one-over-four*)
                                               *one-over-five*)))))))
                    (- *twenty-one-over-twenty*)))
         (res1 (+ (* (- (exp-exp-exp omega1 omega2))
                     (- 1 (exp-exp-exp omega3 omega4)))
                  (- (exp-exp omega5))
                  (- (exp-exp omega6))
                  1.0d0)))
                          
    (declare (type double-float omega1 omega2 omega3 omega4 omega5 omega6 res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (2)) double-float double-float double-float) (simple-array double-float (2))) A))
(defun A (tmp-array x y Cxy)
  (declare (type (simple-array double-float (2)) tmp-array)
           (type double-float x y Cxy))
  (let ((A-part1 (+ 
                  y
                  (* x *one-over-four*)
                  (- (* .25d0
                        (abs
                         (sin
                          (* *twelve-over-five*
                             (+ (* .7d0 (abs (- x (* y *one-over-four*))))
                                (* .3d0 (sqrt (abs (- x (* y *one-over-four*)))))))))))))
        (A-part2 (+ 
                  y
                  (* x *one-over-four*)
                  *seven-over-twenty*
                  (* 0.2d0 (atan (* 6.0d0 (abs (- x (* y *one-over-four*))))))
                  (* 0.2d0 (atan (* 40.0d0 (abs (- x (* y *one-over-four*))))))
                  (- (* *twenty-three-over-twenty*
                        (+ 1.5d0
                           (* *one-over-twenty-five*
                              (cos
                               (* 10.0d0
                                  (+ y
                                     (* x *one-over-four*)
                                     *six-over-twenty-five*))))
                           (* 0.03d0 Cxy)
                           (* 0.3d0
                              (atan (* 30.0d0
                                       (+ y (* x *one-over-four*) (- 0.25d0))))))
                        (abs (- x (* y *one-over-four*))))))))
    (declare (type double-float A-part1 A-part2))
    (loop for v of-type fixnum from 0 to 1
          do
             (setf (aref tmp-array v)
                   (exp
                    (+
                     (- (exp
                         ;; first exponential:
                         (* 200.0d0
                            (+ (* v *one-over-fifty*)
                               A-part1))))
                     (- (exp
                         ;; second exponential:
                         (- (* 200.0d0
                               (+ (- (* (* v 7.0d0) *one-over-fifty*))
                                  A-part2)))))))))
    tmp-array))

(declaim (ftype (function ((simple-array double-float (1)) fixnum double-float double-float) null) K))
(defun K (res v x y)
(declare (type (simple-array double-float (1)) res)
         (type fixnum v)
         (type double-float x y))
  (let ((res1 0.0d0))
    (declare (type double-float res1))
    (loop for s of-type fixnum from 1 to 60
          do (incf res1
                   (* *five-over-two*
                      (+ *two-over-twenty-five*
                         (* *three-over-fifty*
                            (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                      (* (+ (sin (* 5.0d0 s))
                            (sin (* 2.0d0 s))
                            3.0d0)
                         *one-over-five*)
                      (exp-exp
                       (- (* 25
                             (- (* (expt
                                    (sin
                                     (+ (sin (* 2.0d0 s))
                                        (* (+ 6 (sin (* 1.0d0 s s)))
                                           (+ (* (sin (* 7.0d0 s)) (* x *one-over-two*))
                                              (* (cos (* 7.0d0 s)) (* (- y 8) *one-over-two*))))))
                                    10)
                                   (expt
                                    (sin
                                     (+ (sin (* 3.0d0 s))
                                        (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                           (- (* (sin (* 7.0d0 s)) (* (- y 8) *one-over-two*))
                                              (* (cos (* 7.0d0 s)) (* x *one-over-two*))))))
                                    10.0d0))
                                0.1d0)))))))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (3)) double-float double-float double-float double-float double-float (simple-array double-float (2)) (simple-array double-float (1))) (simple-array double-float (3))) H))
(defun H (tmp3 x y Exy Lxy Wxy Axy tmp1)
  (declare (type (simple-array double-float (3)) tmp3)
           (type double-float x y Exy Lxy Wxy)
           (type (simple-array double-float (2)) Axy)
           (type (simple-array double-float (1)) tmp1))
  (let ((H-part1 (* (aref Axy 0)
                    (aref Axy 1)
                    (- 1 Exy)
                    (+ 1.0d0 (* Lxy *one-over-fifty*))
                    (exp-exp (+ (exp
                                 (+ (* 2 y)
                                     (* 0.5d0 x)
                                     *two-over-five*
                                     (- (* 2.0d0 (abs (- x (* y *one-over-four*)))))))
                                 (exp
                                  (+ (* 8 y)
                                     (* 2 x)
                                     *two-over-five*
                                     (- (abs (- (* 8 x) (* 2 y))))))))
                     Wxy))
        (H-part2 (exp-exp
                   (- (* 50.0d0
                         (+ (* (expt (cos (+ (* 2 y) 
                                             (* x 0.5d0)
                                             *seven-over-five*
                                             (- (abs (- (* 2 x)
                                                        (* y 0.5d0))))))
                                     80)
                               (expt (sin (+ (* 20 y)
                                             (* 5 x)
                                             (abs (- (* 20 x) (* 5 y)))))
                                     2))
                            (- (expt (+ (* 2.7d0 y)
                                        (* (* 27 x) *one-over-forty*)
                                        *eighty-one-over-two-hundred-fifty*)
                                     10))
                            (- *forty-nine-over-fifty*)))))))
    (declare (type double-float H-part1 H-part2))
    (loop for v of-type fixnum from 0 to 2
          do
             (progn
               (K tmp1 v x y)
               (let* ((Kvxy (aref tmp1 0))
                      (z (+
                          ;; first term:
                          (* (* (+ 18.0d0 (- (* 9.0d0 v)) (* 1.0d0 v v)) *one-over-twenty*)
                             (- 1.0d0 (aref Axy 0))
                             (- 1.0d0 Exy)
                             Kvxy) 
                          ;; second term:
                          (* (* (+ 2 (* 3.0d0 v)) *one-over-five*) H-part1)
                          ;; third term:
                          H-part2
                          ;; fourth term:
                          (* 0.1d0 Exy (* (- v 1.0d0) (- v 1))))))
                 (declare (type double-float Kvxy z))
                 (setf (aref tmp3 v) z))))
    tmp3))

(declaim (ftype (function (double-float) fixnum) F))
(defun F (z)
  (declare (type double-float z))
  (let ((res (floor
        (* 255.0
           (exp-exp (- (* 1000.0 z)))
           (expt (abs z) (exp-exp (* 1000.0 (- z 1.0))))))))
    (declare (type fixnum res))
    res))
  
(defun main ()
  "Main function. Create butterfly.bmp"

  (let ((start-time (get-internal-real-time)))
    
    (format t "1) Create RGB arrays...~%")
    (let ((export-file (merge-pathnames "butterfly-cl.bmp"))
          (r-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (g-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (b-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0)))
      (declare (type (simple-array fixnum) r-array g-array b-array))

      (format t "2) Calculate RGB components...~%")
      (let ((x 0.0d0)
              (y 0.0d0)
              (Cxy 0.0d0)
              (Exy 0.0d0)
              (Lxy 0.0d0)
              (Wxy 0.0d0)(tmp1 (make-array 1 :initial-element 0.0d0 :element-type 'double-float))
            (Axy (make-array 2 :initial-element 0.0d0 :element-type 'double-float))
            (Hxy (make-array 3 :initial-element 0.0d0 :element-type 'double-float)))
        (declare (type double-float x y Cxy Exy Lxy Wxy)
                 (type (simple-array double-float (1)) tmp1)
                 (type (simple-array double-float (2)) Axy)
                 (type (simple-array double-float (3)) Hxy))
        (loop for n of-type fixnum from 1 to *height* do
              (loop for m of-type fixnum from 1 to *width* do
                    (progn
                      (setq x (/ (- m 1000.0d0) 960.0d0))
                      (setq y (/ (- 451.0d0 n) 960.0d0))
                      (C tmp1 x y)
                      (setq Cxy (aref tmp1 0))
                      (E tmp1 x y)
                      (setq Exy (aref tmp1 0))
                      (L tmp1 x y)
                      (setq Lxy (aref tmp1 0))
                      (W tmp1 x y Cxy)
                      (setq Wxy (aref tmp1 0))
                      (A Axy x y Cxy)
                      (H Hxy x y Exy Lxy Wxy Axy tmp1)
                      (setf (aref r-array (- n 1) (- m 1)) (F (aref Hxy 0)))
                      (setf (aref g-array (- n 1) (- m 1)) (F (aref Hxy 1)))
                      (setf (aref b-array (- n 1) (- m 1)) (F (aref Hxy 2)))

                      ;; print some information:
                      (when (and (= 1 m) (or (= 1 n) (= 0 (mod n 100))))
                        (format t "n = ~a~%" n))))))

      (let* ((end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
        
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Duration: ~f seconds~%" duration))
        
        (format t "3) Export pic...~%")
        ;; (draw-pic-from-rgb-arrays *height* *width* r-array g-array b-array export-file)
        (save-bmp-from-rgb-arrays export-file r-array g-array b-array *width* *height*)
        (format t "Done.~%")))))

;;; (butterfly::main)

;;;; end
