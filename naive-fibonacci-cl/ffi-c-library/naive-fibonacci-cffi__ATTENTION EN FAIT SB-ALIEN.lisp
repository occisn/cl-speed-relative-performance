(defpackage naive-fib-cffi
  (:use :cl))

(in-package :naive-fib-cffi)

;; (declaim (optimize (debug 3) 
;;                    (safety 3)
;;                    (speed 0)))

(sb-alien:load-shared-object "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/naive-fibonacci-cl-cffi/fib.dll")

(sb-alien:define-alien-routine ("naive_fibonacci" c-naive-fibonacci)
    sb-alien:unsigned-int
  (n sb-alien:unsigned-int))

(defun run (n)
  (let* ((start-time (get-internal-real-time))
         (res (c-naive-fibonacci n))
         (end-time (get-internal-real-time))
         (duration (/ (- end-time start-time)
                      internal-time-units-per-second)))
    (format t "Fibonacci(~D) = ~D in ~F seconds~%"
            n res duration)
    (force-output)
    duration))

(defun benchmark-5-times (n)
  "Execute FN 5 times, print each duration, and report the quickest."
  (declare (type fixnum n))
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (run n)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (let ((quickest (apply #'min durations))
          (slowest (apply #'max durations)))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "~%RESULTS:~%")
        (dotimes (i nb-runs)
          (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs (nth (- nb-runs 1 i) durations)))
        (format t "=> quickest time: ~F seconds~%" quickest)
        (format t "=> slowest time:  ~F seconds = quickest + ~a %~%" slowest (truncate (* 100 (/ (- slowest quickest) quickest)))))
      nil)))

(defun main ()
  (let ((n 46))
    (declare (type fixnum n))
    ;; (run n)
    (benchmark-5-times n) ; 4.4 s
    ))

;;; end
