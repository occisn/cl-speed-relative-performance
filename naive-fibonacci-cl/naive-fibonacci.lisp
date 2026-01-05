(eval-when (:compile-toplevel)
  (ql:quickload :cffi))

(defpackage naive-fib
  (:use :cl :cffi))

(in-package naive-fib)

(declaim (optimize (speed 3)
                   (debug 0) 
                   (safety 0)
                   (space 0)))

(declaim (ftype (function (fixnum) fixnum) naive-fibonacci))
(defun naive-fibonacci-recursive (n)
  (declare (type fixnum n))
  (if (< n 2)
      n
      (the fixnum (+ (naive-fibonacci-recursive (the fixnum (- n 1)))
                     (naive-fibonacci-recursive (the fixnum (- n 2)))))))

(defun naive-fibonacci-1 (n)
  (let* ((start-time (get-internal-real-time))
         (res (naive-fibonacci-recursive n))
         (end-time (get-internal-real-time))
         (duration (/ (- end-time start-time)
                      internal-time-units-per-second)))
    (format t "Fibonacci(~D) = ~D in ~F seconds~%"
            n res duration)
    (force-output)
    duration))

(defun naive-fibonacci-1--benchmark-5-times (n)
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (naive-fibonacci-1 n)))
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

(defun naive-fibonacci-2 (n)
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (sb-alien:load-shared-object "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/naive-fibonacci-cl/ffi-c-library/fib.dll")
  
  (sb-alien:define-alien-routine
      ("naive_fibonacci" c-naive-fibonacci-2)
    sb-alien:double
    (n sb-alien:unsigned-int))

  (let ((duration (c-naive-fibonacci-2 n)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun naive-fibonacci-2--benchmark-5-times (n)
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (naive-fibonacci-2 n)))
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

(defun naive-fibonacci-3 (n)
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (cffi:define-foreign-library libfibonacci
    (:windows "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/naive-fibonacci-cl/ffi-c-library/fib.dll"))

  (cffi:use-foreign-library libfibonacci)

  (cffi:defcfun ("naive_fibonacci" c-naive-fibonacci-3)
      :double
    (n :uint32))

  (let ((duration (c-naive-fibonacci-3 n)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun naive-fibonacci-3--benchmark-5-times (n)
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (naive-fibonacci-3 n)))
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
  (let ((n 46))                         ; 1836311903
    (declare (type fixnum n))
    ;; (naive-fibonacci-1 n)
    ;; (naive-fibonacci-1--benchmark-5-times n)
    ;; (naive-fibonacci-2 n)
    ;; (naive-fibonacci-2--benchmark-5-times n)
    ;; (naive-fibonacci-3 n)
    (naive-fibonacci-3--benchmark-5-times n)
    ))


;;; end
