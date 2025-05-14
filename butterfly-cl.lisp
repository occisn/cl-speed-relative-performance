(ql:quickload :lparallel)

(defpackage butterfly
  (:use :cl ))

(in-package :butterfly)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defparameter *width* 2000
  "width of the picture, in pixels")
(defparameter *height* 1100
  "height of the picture, in pixels")
(declaim (type fixnum *width* *height*))

;; frequent fractions (to avoid frequent divisions)
(defparameter *f-1-2* 0.5d0)
(declaim (type double-float *f-1-2*))
(defparameter *f-1-3* (/ 1.0d0 3.0d0))
(declaim (type double-float *f-1-3*))
(defparameter *f-1-4* 0.25d0)
(declaim (type double-float *f-1-4*))
(defparameter *f-1-8* 0.125d0)
(declaim (type double-float *f-1-8*))
(defparameter *f-1-20* (/ 1.0d0 20.0d0))
(declaim (type double-float *f-1-20*))
(defparameter *f-1-25* (/ 1.0d0 25.0d0))
(declaim (type double-float *f-1-25*))
(defparameter *f-1-5* 0.2d0)
(declaim (type double-float *f-1-5*))
(defparameter *f-1-40* (/ 1.0d0 40.0d0))
(declaim (type double-float *f-1-40*))
(defparameter *f-1-50* (/ 1.0d0 50.0d0))
(declaim (type double-float *f-1-50*))
(defparameter *f-1-200* (/ 1.0d0 200.0d0))
(declaim (type double-float *f-1-200*))
(defparameter *f-2-5* (/ 2.0d0 5.0d0))
(declaim (type double-float *f-2-5*))
(defparameter *f-2-25* (/ 2.0d0 25.0d0))
(declaim (type double-float *f-2-25*))
(defparameter *f-3-50* (/ 3.0d0 50.0d0))
(declaim (type double-float *f-3-50*))
(defparameter *f-4-5* 0.8d0)
(declaim (type double-float *f-4-5*))
(defparameter *f-5-2* 2.5d0)
(declaim (type double-float *f-5-2*))
(defparameter *f-6-25* (/ 6.0d0 25.0d0))
(declaim (type double-float *f-6-25*))
(defparameter *f-7-5* 1.4d0)
(declaim (type double-float *f-7-5*))
(defparameter *f-7-20* (/ 7.0d0 20.0d0))
(declaim (type double-float *f-7-20*))
(defparameter *f-7-50* (/ 7.0d0 50.0d0))
(declaim (type double-float *f-7-50*))
(defparameter *f-8-5* 1.6d0)
(declaim (type double-float *f-8-5*))
(defparameter *f-9-20* (/ 9.0d0 20.0d0))
(declaim (type double-float *f-9-20*))
(defparameter *f-12-5* (/ 12.0d0 5.0d0))
(declaim (type double-float *f-12-5*))
(defparameter *f-21-20* (/ 21.0d0 20.0d0))
(declaim (type double-float *f-21-20*))
(defparameter *f-23-20* (/ 23.0d0 20.0d0))
(declaim (type double-float *f-23-20*))
(defparameter *f-49-50* (/ 49.0d0 50.0d0))
(declaim (type double-float *f-49-50*))
(defparameter *f-81-250* (/ 81.0d0 250.0d0))
(declaim (type double-float *f-81-250*))
(defparameter *f-196-5* (/ 196.0d0 5.0d0))
(declaim (type double-float *f-196-5*))

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
  "Save the RGB data from the 2D arrays R-ARRAY, G-ARRAY, B-ARRAY to a BMP file.
(proposed by ChatGTP and kept untouched since not involved in performance comparison)"
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
  "Calculate C(x,y) and store the result in 1-cell array RES."
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y))
  (let ((res1 (expt
               (sin (+ (* 14 (atan (/ (* 100 (+ y
                                                (* x *f-1-4*)
                                                (- *f-1-25*)))
                                      (+ (abs (- (* 100.0d0 x)
                                                 (* 25.0d0 y)
                                                 (* 3.0d0 (atan (- (* 100.0d0 x) (* 25.0d0 y))))))
                                         1.0d0))))
                       (* 14 (abs (- (* x *f-1-2*)
                                     (* y *f-1-8*))))))
               4)))
    (declare (type double-float res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float) null) E))
(defun E (res x y)
  "Calculate E(x,y) and store the result in 1-cell array RES."
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
                                                   (- x (* y *f-1-4*))))
                                               (+ (* 3 y) (* 0.75d0 x) 2.27d0))))))
                    (- 1.0d0
                       (exp-exp-exp
                        (- (* 200
                              (abs
                               (+ y
                                  (* x *f-1-4*)
                                  (- 0.2d0)
                                  (* 3 (- x (* y *f-1-4*)) (- x (* y *f-1-4*))))))
                           32.0d0)
                        (- (* 500
                              (abs
                               (+ y
                                  (* x *f-1-4*)
                                  (- *f-1-20*)
                                  (- (* 0.7d0 (sqrt (abs (- x (* y *f-1-4*)))))))))
                           2.5d0)))))))
    (declare (type double-float res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float) null) L))
(defun L (res x y)
  "Calculate L(x,y) and store the result in 1-cell array RES."
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
                            (abs (- (* x *f-1-2*) (* y *f-1-8*)))
                            (* 4.0d0 (sin (* 5.0d0 s)))))
                    6)))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (1)) double-float double-float double-float) null) W))
(defun W (res x y Cxy)
  "Calculate W(x,y) and store the result in 1-cell array RES."
  (declare (type (simple-array double-float (1)) res)
           (type double-float x y Cxy))
  (let* ((omega1 (+ (- (* 40 Cxy))
                   *f-196-5*
                    (* *f-4-5*
                       (sqrt (the (double-float 0.0d0)
                                  (+ (* (- x (* y *f-1-4*))
                                        (- x (* y *f-1-4*)))
                                     (* (+ y (* x *f-1-4*))
                                        (+ y (* x *f-1-4*)))))))))
         (omega2 (- (* 40
                       (+ (* 5 (abs
                                (+ y
                                   (* x *f-1-4*)
                                   (- *f-3-50*)
                                   (* *f-1-3*
                                      (- x (* y *f-1-4*))
                                      (- x (* y *f-1-4*))))))
                          (expt (abs (- (* 2 x) (* y *f-1-2*))) 3)
                          (- *f-2-5*)))))
         (omega3 (+
                  (- (* 1000
                        (+ (abs (- x (* y *f-1-4*))))))
                  100.0d0
                  (- (* 90.0d0 (atan (+ (* 8 y)
                                        (* 2 x)
                                        *f-8-5*))))))
         (omega4 (* 1000 (+ (abs (- x (* y *f-1-4*)))
                            (- *f-7-50*)
                            (* *f-9-20*
                               (+ y (* x *f-1-4*) 0.2d0)))))
         (omega5 (* 70 (+ (abs
                           (+
                            (* 5
                               (abs
                                (+ y
                                   (* x *f-1-4*)
                                   (- *f-3-50*)
                                   (* *f-1-3*
                                      (- x (* y *f-1-4*))
                                      (- x (* y *f-1-4*))))))
                            (expt (abs (- (* 2 x) (* y *f-1-2*))) 3)
                            (- *f-2-5*)))
                          (- *f-1-200*))))
         (omega6 (+ (* 700
                       (abs
                        (+
                         (abs (- x (* y *f-1-4*)))
                         (- 0.1d0)
                         (* 0.09d0 (atan (* 8
                                            (+ y
                                               (* x *f-1-4*)
                                               *f-1-5*)))))))
                    (- *f-21-20*)))
         (res1 (+ (* (- (exp-exp-exp omega1 omega2))
                     (- 1 (exp-exp-exp omega3 omega4)))
                  (- (exp-exp omega5))
                  (- (exp-exp omega6))
                  1.0d0)))
                          
    (declare (type double-float omega1 omega2 omega3 omega4 omega5 omega6 res1))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (2)) double-float double-float double-float) (simple-array double-float (2))) A))
(defun A (res2 x y Cxy)
  "Calculate A(v,x,y) for v=0,1 and store the result in 2-cell array RES2."
  (declare (type (simple-array double-float (2)) res2)
           (type double-float x y Cxy))
  (let ((A-part1 (+ 
                  y
                  (* x *f-1-4*)
                  (- (* .25d0
                        (abs
                         (sin
                          (* *f-12-5*
                             (+ (* .7d0 (abs (- x (* y *f-1-4*))))
                                (* .3d0 (sqrt (abs (- x (* y *f-1-4*)))))))))))))
        (A-part2 (+ 
                  y
                  (* x *f-1-4*)
                  *f-7-20*
                  (* 0.2d0 (atan (* 6.0d0 (abs (- x (* y *f-1-4*))))))
                  (* 0.2d0 (atan (* 40.0d0 (abs (- x (* y *f-1-4*))))))
                  (- (* *f-23-20*
                        (+ 1.5d0
                           (* *f-1-25*
                              (cos
                               (* 10.0d0
                                  (+ y
                                     (* x *f-1-4*)
                                     *f-6-25*))))
                           (* 0.03d0 Cxy)
                           (* 0.3d0
                              (atan (* 30.0d0
                                       (+ y (* x *f-1-4*) (- 0.25d0))))))
                        (abs (- x (* y *f-1-4*))))))))
    (declare (type double-float A-part1 A-part2))
    (loop for v of-type fixnum from 0 to 1
          do
             (setf (aref res2 v)
                   (exp
                    (+
                     (- (exp
                         ;; first exponential:
                         (* 200.0d0
                            (+ (* v *f-1-50*)
                               A-part1))))
                     (- (exp
                         ;; second exponential:
                         (- (* 200.0d0
                               (+ (- (* (* v 7.0d0) *f-1-50*))
                                  A-part2)))))))))
    res2))

(declaim (ftype (function ((simple-array double-float (1)) fixnum double-float double-float) null) K))
(defun K (res v x y)
  "Calculate K(v,x,y) and store the result in 1-cell array RES."
  (declare (type (simple-array double-float (1)) res)
           (type fixnum v)
           (type double-float x y))
  (let ((res1 0.0d0))
    (declare (type double-float res1))
    (loop for s of-type fixnum from 1 to 60
          do (incf res1
                   (* *f-5-2*
                      (+ *f-2-25*
                         (* *f-3-50*
                            (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                      (* (+ (sin (* 5.0d0 s))
                            (sin (* 2.0d0 s))
                            3.0d0)
                         *f-1-5*)
                      (exp-exp
                       (- (* 25
                             (- (* (expt
                                    (sin
                                     (+ (sin (* 2.0d0 s))
                                        (* (+ 6 (sin (* 1.0d0 s s)))
                                           (+ (* (sin (* 7.0d0 s)) (* x *f-1-2*))
                                              (* (cos (* 7.0d0 s)) (* (- y 8) *f-1-2*))))))
                                    10)
                                   (expt
                                    (sin
                                     (+ (sin (* 3.0d0 s))
                                        (* (+ 6 (* 2 (sin (* 1.0d0 s s))))
                                           (- (* (sin (* 7.0d0 s)) (* (- y 8) *f-1-2*))
                                              (* (cos (* 7.0d0 s)) (* x *f-1-2*))))))
                                    10.0d0))
                                0.1d0)))))))
    (setf (aref res 0) res1)
    nil))

(declaim (ftype (function ((simple-array double-float (3)) double-float double-float double-float double-float double-float (simple-array double-float (2)) (simple-array double-float (1))) (simple-array double-float (3))) H))
(defun H (res3 x y Exy Lxy Wxy Axy tmp1)
  "Calculate H(v,x,y) for v=0,1,2 and store the result in 3-cell array RES3.
TMP1 is a temporary 1-cell array."
  (declare (type (simple-array double-float (3)) res3)
           (type double-float x y Exy Lxy Wxy)
           (type (simple-array double-float (2)) Axy)
           (type (simple-array double-float (1)) tmp1))
  (let ((H-part1 (* (aref Axy 0)
                    (aref Axy 1)
                    (- 1 Exy)
                    (+ 1.0d0 (* Lxy *f-1-50*))
                    (exp-exp (+ (exp
                                 (+ (* 2 y)
                                     (* 0.5d0 x)
                                     *f-2-5*
                                     (- (* 2.0d0 (abs (- x (* y *f-1-4*)))))))
                                 (exp
                                  (+ (* 8 y)
                                     (* 2 x)
                                     *f-2-5*
                                     (- (abs (- (* 8 x) (* 2 y))))))))
                     Wxy))
        (H-part2 (exp-exp
                   (- (* 50.0d0
                         (+ (* (expt (cos (+ (* 2 y) 
                                             (* x 0.5d0)
                                             *f-7-5*
                                             (- (abs (- (* 2 x)
                                                        (* y 0.5d0))))))
                                     80)
                               (expt (sin (+ (* 20 y)
                                             (* 5 x)
                                             (abs (- (* 20 x) (* 5 y)))))
                                     2))
                            (- (expt (+ (* 2.7d0 y)
                                        (* (* 27 x) *f-1-40*)
                                        *f-81-250*)
                                     10))
                            (- *f-49-50*)))))))
    (declare (type double-float H-part1 H-part2))
    (loop for v of-type fixnum from 0 to 2
          do
             (progn
               (K tmp1 v x y)
               (let* ((Kvxy (aref tmp1 0))
                      (z (+
                          ;; first term:
                          (* (* (+ 18.0d0 (- (* 9.0d0 v)) (* 1.0d0 v v)) *f-1-20*)
                             (- 1.0d0 (aref Axy 0))
                             (- 1.0d0 Exy)
                             Kvxy) 
                          ;; second term:
                          (* (* (+ 2 (* 3.0d0 v)) *f-1-5*) H-part1)
                          ;; third term:
                          H-part2
                          ;; fourth term:
                          (* 0.1d0 Exy (* (- v 1.0d0) (- v 1))))))
                 (declare (type double-float Kvxy z))
                 (setf (aref res3 v) z))))
    res3))

(declaim (ftype (function (double-float) fixnum) F))
(defun F (z)
  "Calculate F(z)."
  (declare (type double-float z))
  (let ((res (floor
        (* 255.0
           (exp-exp (- (* 1000.0 z)))
           (expt (abs z) (exp-exp (* 1000.0 (- z 1.0))))))))
    (declare (type fixnum res))
    res))
  
(defun butterfly-1 ()
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

(defun butterfly-2 ()
  "Main function. Create butterfly.bmp"

  (let ((start-time (get-internal-real-time)))
    
    (format t "1) Create RGB arrays...~%")
    (let* ((export-file (merge-pathnames "butterfly-cl.bmp"))
          (r-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (g-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (b-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0)))
      
      (declare (type (simple-array fixnum) r-array g-array b-array))
      
      (format t "2) Calculate RGB components...~%")
      (setq lparallel:*kernel* (lparallel:make-kernel 8))
      (let* ((nb-chunks 30)
             (chunk-size (ceiling *height* nb-chunks))
             (starts (make-array nb-chunks :element-type 'fixnum :initial-contents
                                 (loop for j of-type fixnum from 1 to nb-chunks
                                       for start of-type fixnum = 0 then (+ start chunk-size)
                                       collect start)))
             (partial-sums
             (lparallel:pmap
              '(simple-array fixnum (*))
              (lambda (start)
                (declare (type fixnum start))
                (let ((end (min (- *height* 1) (+ start chunk-size))))
                  (declare (type fixnum end))
                  (loop for n of-type fixnum from (+ 1 start) below (+ 1 end)
                        do
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
                  1))
              starts))
             )
        
        (declare (type fixnum nb-chunks chunk-size)
                 (type (simple-array fixnum (*)) starts)
                 (type vector partial-sums)
                 (ignore partial-sums)))

      (let* ((end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
        
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Duration: ~f seconds~%" duration))
        
        (format t "3) Export pic...~%")
        ;; (draw-pic-from-rgb-arrays *height* *width* r-array g-array b-array export-file)
        (save-bmp-from-rgb-arrays export-file r-array g-array b-array *width* *height*)
        (format t "Done.~%")))))


(defun butterfly-3 ()
  "Main function. Create butterfly.bmp"

  (let ((start-time (get-internal-real-time)))
    
    (format t "1) Create RGB arrays...~%")
    (let* ((export-file (merge-pathnames "butterfly-cl.bmp"))
          (r-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (g-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0))
          (b-array (make-array `(,*height* ,*width*) :element-type 'fixnum :initial-element 0)))
      
      (declare (type (simple-array fixnum) r-array g-array b-array))
      
      (format t "2) Calculate RGB components...~%")
      (setq lparallel:*kernel* (lparallel:make-kernel 8))
      (let* ((nb-chunks 30)
             (chunk-size (ceiling *height* nb-chunks))
             (threads (loop for i from 0 below nb-chunks
                        collect (sb-thread:make-thread
	                         (lambda (i)
                                   (declare (type fixnum i))
                                   (let* ((start (* i chunk-size))
                                          (end (min (- *height* 1) (+ start chunk-size))))
                                     (declare (type fixnum start end))
                                     ;; (format t "from ~a to ~a~%" (+ 1 start) (+ 1 end))
                                     (loop for n of-type fixnum from (+ 1 start) below (+ 1 end)
                                           do
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
                                                      (format t "n = ~a~%" n))))))))
                                 :arguments (list i)))))
        
        (declare (type fixnum nb-chunks chunk-size))

        (dolist (thread threads)
          (sb-thread:join-thread thread)))

      (let* ((end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
        
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Duration: ~f seconds~%" duration))
        
        (format t "3) Export pic...~%")
        ;; (draw-pic-from-rgb-arrays *height* *width* r-array g-array b-array export-file)
        (save-bmp-from-rgb-arrays export-file r-array g-array b-array *width* *height*)
        (format t "Done.~%")))))

;;;; end
