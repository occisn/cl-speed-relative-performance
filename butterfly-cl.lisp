
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
(defparameter *seven-over-five* 1.4d0)
(declaim (type double-float *seven-over-five*))

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


(defun main ()
  "Main function. Create butterfly.png"

  (let ((start-time (get-internal-real-time)))
    
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
            (let* ((x 0.0d0)
                   (y 0.0d0)
                   (Cxy 0.0d0)
                   (Exy 0.0d0)
                   (Lxy 0.0d0)
                   (Wxy 0.0d0)
                   (A-part1 0.0d0)
                   (A-part2 0.0d0)
                   (A0xy 0.0d0)
                   (A1xy 0.0d0)
                   (Kvxy 0.0d0)
                   (H-part1 0.0d0)
                   (H-part2 0.0d0)
                   (Hvxy 0.0d0)
                   (v 0)
                   (r 0)
                   (g 0)
                   (b 0))
              (declare (type double-float x y Cxy Exy Lxy Wxy A-part1 A-part2 A0xy A1xy Kvxy H-part1 H-part2 Hvxy)
                       (type fixnum v r g b))

              ;; x and y
              
              (setq x (/ (- m 1000.0d0) 960.0d0))

              (setq y (/ (- 451.0d0 n) 960.0d0))

              ;; Cxy
              
              (setq Cxy (expt
                         (sin (+ (* 14 (atan (/ (* 100 (+ y
                                                          (* x *one-over-four*)
                                                          (- *one-over-twenty-five*)))
                                                (+ (abs (- (* 100.0d0 x)
                                                           (* 25.0d0 y)
                                                           (* 3.0d0 (atan (- (* 100.0d0 x) (* 25.0d0 y))))))
                                                   1.0d0))))
                                 (* 14 (abs (- (* x *one-over-two*)
                                               (* y *one-over-eight*))))))
                         4))
              ;; Exy

              (setq Exy
                    (- 1.0d0
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
                                 2.5d0))))))
              

              ;; Lxy

              (setq Lxy 0.0d0)
              (loop for s of-type fixnum from 1 to 25
                    do
                       (incf Lxy
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

              ;; Wxy

              (setq Wxy (let* ((omega1 (+ (- (* 40 Cxy))
                                          (/ 196.0d0 5.0d0)
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
                                                              (/ 8 5.0d0)))))))
                               (omega4 (* 1000 (+ (abs (- x (* y *one-over-four*)))
                                                  (- (/ 7.0d0 50.0d0))
                                                  (* (/ 9.0d0 20.0d0)
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
                                                (- (/ 1.0d0 200.0d0)))))
                               (omega6 (+ (* 700
                                             (abs
                                              (+
                                               (abs (- x (* y *one-over-four*)))
                                               (- 0.1d0)
                                               (* 0.09d0 (atan (* 8
                                                                  (+ y
                                                                     (* x *one-over-four*)
                                                                     (/ 1 5.0d0))))))))
                                          (- (/ 21 20.0d0)))))
                          
                          (declare (type double-float omega1 omega2 omega3 omega4 omega5 omega6))
                          
                          (+ (* (- (exp-exp-exp omega1 omega2))
                                (- 1 (exp-exp-exp omega3 omega4)))
                             (- (exp-exp omega5))
                             (- (exp-exp omega6))
                             1.0d0)))

              ;; Axy

              (setq A-part1 (+ 
                             y
                             (* x *one-over-four*)
                             (- (* .25d0
                                   (abs
                                    (sin
                                     (* (/ 12.0d0 5.0d0)
                                        (+ (* .7d0 (abs (- x (* y *one-over-four*))))
                                           (* .3d0 (sqrt (abs (- x (* y *one-over-four*)))))))))))))
              
              (setq A-part2 (+ 
                             y
                             (* x *one-over-four*)
                             (/ 7.0d0 20.0d0)
                             (* 0.2d0 (atan (* 6.0d0 (abs (- x (* y *one-over-four*))))))
                             (* 0.2d0 (atan (* 40.0d0 (abs (- x (* y *one-over-four*))))))
                             (- (* (/ 23.0d0 20.0d0)
                                   (+ 1.5d0
                                      (* *one-over-twenty-five*
                                         (cos
                                          (* 10.0d0
                                             (+ y
                                                (* x *one-over-four*)
                                                (/ 6.0d0 25.0d0)))))
                                      (* 0.03d0 Cxy)
                                      (* 0.3d0
                                         (atan (* 30.0d0
                                                  (+ y (* x *one-over-four*) (- 0.25d0))))))
                                   (abs (- x (* y *one-over-four*)))))))

              (setq v 0)
              (setq A0xy (exp
                          (+
                           (- (exp
                               ;; first exponential:
                               (* 200.0d0
                                  (+ (/ v 50.0d0)
                                     A-part1))))
                           (- (exp
                               ;; second exponential:
                               (- (* 200.0d0
                                     (+ (- (/ (* v 7.0d0) 50.0d0))
                                        A-part2))))))))

              (setq v 1)
              (setq A1xy (exp
                          (+
                           (- (exp
                               ;; first exponential:
                               (* 200.0d0
                                  (+ (/ v 50.0d0)
                                     A-part1))))
                           (- (exp
                               ;; second exponential:
                               (- (* 200.0d0
                                     (+ (- (/ (* v 7.0d0) 50.0d0))
                                        A-part2))))))))

              ;; parts of H

              (setq H-part1 (* A0xy
                               A1xy
                               (- 1 Exy)
                               (/ (+ 50.0d0 Lxy) 50.0d0)
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

              (setq H-part2 (exp-exp
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
                                                  (/ (* 27 x) 40.0d0)
                                                  (/ 81.0d0 250.0d0)
                                                  )
                                               10))
                                      (- (/ 49.0d0 50.0d0)))))))

              ;; Kvxy, Hvxy, F and RGB

              (setq v 0)
                          (setq Kvxy 0.0d0)
              (loop for s of-type fixnum from 1 to 60
                    do (incf Kvxy
                             (* *five-over-two*
                                (+ *two-over-twenty-five*
                                   (* *three-over-fifty*
                                      (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                (/ (+ (sin (* 5.0d0 s))
                                      (sin (* 2.0d0 s))
                                      3.0d0)
                                   5.0d0)
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
              (setq Hvxy (+
                         ;; first term:
                         (* (/ (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) 20.0d0)
                            (- 1.0d0 A0xy)
                            (- 1.0d0 Exy)
                            Kvxy) 
                         ;; second term:
                         (* (/ (+ 2 (* 3.0d0 v)) 5.0d0) H-part1)
                         ;; third term:
                         H-part2
                         ;; fourth term:
                         (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))
              (setq r (floor
                       (* 255.0
                          (exp-exp (- (* 1000.0 Hvxy)))
                          (expt (abs Hvxy) (exp-exp (* 1000.0 (- Hvxy 1.0)))))))

              (setq v 1)
                          (setq Kvxy 0.0d0)
              (loop for s of-type fixnum from 1 to 60
                    do (incf Kvxy
                             (* *five-over-two*
                                (+ *two-over-twenty-five*
                                   (* *three-over-fifty*
                                      (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                (/ (+ (sin (* 5.0d0 s))
                                      (sin (* 2.0d0 s))
                                      3.0d0)
                                   5.0d0)
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
              (setq Hvxy (+
                         ;; first term:
                         (* (/ (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) 20.0d0)
                            (- 1.0d0 A0xy)
                            (- 1.0d0 Exy)
                            Kvxy) 
                         ;; second term:
                         (* (/ (+ 2 (* 3.0d0 v)) 5.0d0) H-part1)
                         ;; third term:
                         H-part2
                         ;; fourth term:
                         (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))
              (setq g (floor
                       (* 255.0
                          (exp-exp (- (* 1000.0 Hvxy)))
                          (expt (abs Hvxy) (exp-exp (* 1000.0 (- Hvxy 1.0)))))))

              (setq v 2)
                          (setq Kvxy 0.0d0)
              (loop for s of-type fixnum from 1 to 60
                    do (incf Kvxy
                             (* *five-over-two*
                                (+ *two-over-twenty-five*
                                   (* *three-over-fifty*
                                      (cos (* s (+ 4.0d0 (* 4.0d0 v))))))
                                (/ (+ (sin (* 5.0d0 s))
                                      (sin (* 2.0d0 s))
                                      3.0d0)
                                   5.0d0)
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
              (setq Hvxy (+
                         ;; first term:
                         (* (/ (+ 18 (- (* 9.0d0 v)) (* 1.0d0 v v)) 20.0d0)
                            (- 1.0d0 A0xy)
                            (- 1.0d0 Exy)
                            Kvxy) 
                         ;; second term:
                         (* (/ (+ 2 (* 3.0d0 v)) 5.0d0) H-part1)
                         ;; third term:
                         H-part2
                         ;; fourth term:
                         (* 0.1d0 Exy (* (- v 1.0d0) (- v 1)))))
              (setq b (floor
                       (* 255.0
                          (exp-exp (- (* 1000.0 Hvxy)))
                          (expt (abs Hvxy) (exp-exp (* 1000.0 (- Hvxy 1.0)))))))
              
              (setf (aref r-array (- n 1) (- m 1)) r)
              (setf (aref g-array (- n 1) (- m 1)) g)
              (setf (aref b-array (- n 1) (- m 1)) b)

              (when (and (= 200 m) (= 200 n))
                (locally
                    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                  (format t "Cxy(n = ~a, m = ~a) = ~a~%" n m Cxy)
                  (format t "Exy(n = ~a, m = ~a) = ~a~%" n m Exy)
                  (format t "Lxy(n = ~a, m = ~a) = ~a~%" n m Lxy)
                  (format t "Wxy(n = ~a, m = ~a) = ~a~%" n m Wxy)
                  (format t "A-part1(n = ~a, m = ~a) = ~a~%" n m A-part1)
                  (format t "A-part2(n = ~a, m = ~a) = ~a~%" n m A-part2)
                  (format t "A0xy(n = ~a, m = ~a) = ~a~%" n m A0xy)
                  (format t "A1xy(n = ~a, m = ~a) = ~a~%" n m A1xy)
                  ))
              
              )

            ;; print some information:
            (when (and (= 1 m) (or (= 1 n) (= 0 (mod n 100))))
              (format t "n = ~a~%" n)))))

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
