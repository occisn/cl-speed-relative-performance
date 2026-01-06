(eval-when (:compile-toplevel)
  (ql:quickload :cffi))

(defpackage loops
  (:use :cl :cffi))

(in-package loops)

(declaim (optimize (debug 0) 
                   (safety 0)
                   (speed 3)))

(defparameter *n* 10000)   ; 10 K
(defparameter *m* 1000000) ; 1 M
(defparameter *u* 40)
(declaim (type fixnum *n* *m* *u*))


(defun loops-1 ()
  "Run loops with timing and return duration."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((start (get-internal-real-time)))

    (let* ((sum 0)
         (r (random 10000)) ;; Random number 0..9999
         (a (make-array *n* :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 32) (*)) a))
    (declare (type fixnum sum r))
    ;; (format t "r = ~a~%" r)
    (loop for i of-type fixnum from 1 below *n*
          do (progn
               (loop for j of-type fixnum from 0 below *m*
                     do (setf (aref a i)
                              (+ (aref a i)
                                 (the fixnum (mod (the fixnum (+ j r)) *u*)))))
               (incf (aref a i) r)
               (incf sum (aref a i)))) 
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "sum = ~a~%" sum)))
    
    (let ((end (get-internal-real-time)))
      ;; SBCL internal time unit is 1/10,000 of a second
      (let ((duration (/ (- end start) internal-time-units-per-second)))
        (format t "Done in ~f seconds~%" duration)
        duration))))

(defun loops-1--benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (loops-1)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq durations (sort durations #'<)))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun loops-2 ()
  "Run loops with timing and return duration."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((start (get-internal-real-time)))

    (let* ((sum 0)
         (r (random 10000)) ;; Random number 0..9999
         (a (make-array *n* :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (dynamic-extent a))
    (declare (type (simple-array (unsigned-byte 32) (*)) a))
    (declare (type fixnum sum r))
    ;; (format t "r = ~a~%" r)
    (loop for i of-type fixnum from 1 below *n*
          do (progn
               (loop for j of-type fixnum from 0 below *m*
                     do (setf (aref a i)
                              (+ (aref a i)
                                 (the fixnum (mod (the fixnum (+ j r)) *u*)))))
               (incf (aref a i) r)
               (incf sum (aref a i)))) 
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "sum = ~a~%" sum)))
    
    (let ((end (get-internal-real-time)))
      ;; SBCL internal time unit is 1/10,000 of a second
      (let ((duration (/ (- end start) internal-time-units-per-second)))
        (format t "Done in ~f seconds~%" duration)
        duration))))

(defun loops-2--benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (loops-2)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq durations (sort durations #'<)))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun loops-A ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (sb-alien:load-shared-object "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/loops-cl/ffi-c-library/ffi.dll")
  
  (sb-alien:define-alien-routine
      ("run" c-loops-A)
      sb-alien:double)

  (let ((duration (c-loops-A)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun loops-A--benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (loops-A)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq durations (sort durations #'<)))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun loops-B ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (cffi:define-foreign-library libloops
    (:windows "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/loops-cl/ffi-c-library/ffi.dll"))

  (cffi:use-foreign-library libloops)

  (cffi:defcfun ("run" c-loops-B)
      :double)

  (let ((duration (c-loops-B)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun loops-B--benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (loops-B)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setq durations (sort durations #'<)))
    (let ((quickest (car durations))
          (second-best (cadr durations))
          (slowest (car (last durations))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest duration: ~,4F seconds~%" quickest)
        (format t "=> second best:       ~,4F seconds~%" second-best)
        (format t "=> slowest duration:  ~,4F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun main ()
  
  (format t "~%LOOPS-1~%-------~%~%")
  ;; (loops-1)
  (loops-1--benchmark-5-times)

  (format t "~%LOOPS-2~%-------~%~%")
  ;; (loops-2)
  (loops-2--benchmark-5-times)

  (format t "~%LOOPS-A~%-------~%~%")
  ;; (loops-A)
  (loops-A--benchmark-5-times)

  (format t "~%LOOPS-B~%-------~%~%")
  ;; (loops-B)
  (loops-B--benchmark-5-times))

;; end
