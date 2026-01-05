;;; C-c C-k to compile file 

(eval-when (:compile-toplevel)
  (ql:quickload :lparallel)
  (ql:quickload :cffi)
  (require 'sb-concurrency))

(defpackage leibniz
  (:use :cl :cffi :lparallel))

(in-package :leibniz)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +n+ 10000000000) ; 10 M
(defconstant +nb-cores+ 8)
(declaim (type fixnum +n+ +nb-cores+))

(defun leibniz-1 () 
  ""
  (let ((start-time (get-internal-real-time))
        (n +n+)
        (tmp 0.0d0))
    (declare (type fixnum n)
             (type double-float tmp))
    (dotimes (i n)
      (declare (type fixnum i))
      (let ((sign (if (evenp i) 1.0d0 -1.0d0)))
        (incf tmp (* sign (/ 1.0d0 (the fixnum (+ (* 2 i) 1)))))))
    (setq tmp (* 4 tmp))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (let* ((end-time (get-internal-real-time))
             (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
        (format t "Leibniz formula: pi = ~,20f in ~f seconds~%" tmp duration)
        duration))))

(defun leibniz-1-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (leibniz-1)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
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

(defun leibniz-2 ()
  ""
  (let* ((n +n+)
         (nb-cores +nb-cores+)
         (nb-chunks (* 4 nb-cores))
         (start-time (get-internal-real-time))
         (chunk-size (ceiling n nb-chunks)))
    (declare (type fixnum n chunk-size)
             (type fixnum nb-cores nb-chunks))

    (setq lparallel:*kernel* (lparallel:make-kernel nb-cores))

    (let ((partial-sums
            (lparallel:pmap
             'list
             (lambda (chunk-idx)
               (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) ; to avoid float to pointer coercion (cost 13) from PARTIAL-SUM to "<return value>"
               (declare (type fixnum chunk-idx))
               (let* ((chunk-start (* chunk-idx chunk-size))
                      (chunk-end  (min (the fixnum (- n 1))
                                       (the fixnum (- (+ chunk-start chunk-size) 1))))
                      (partial-sum 0.0d0))
                 (declare (type fixnum chunk-start chunk-end)
                          (type double-float partial-sum))
                 (loop for i of-type fixnum from chunk-start to chunk-end do
                   (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0) (+ (* 2.0d0 i) 1.0d0))))
                 partial-sum))
             (loop for i from 0 below nb-chunks collect i))))

      (let ((res (* 4.0d0 (the double-float (reduce #'+ partial-sums))))
            (duration (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)))
        (declare (type double-float res))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (format t "Leibniz formula: ~D chunks, pi = ~20,20F (in ~F s)~%"
                  nb-chunks res duration)
          duration)))))

(defun leibniz-2-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (leibniz-2)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
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

(defun leibniz-3 ()
  ""
  
  (let* ((n +n+)
         (nb-cores +nb-cores+)
         (nb-chunks (* 4 nb-cores))
         (start-time (get-internal-real-time))
         (chunk-size (ceiling n nb-chunks))
         (queue (sb-concurrency:make-queue))
         (workers '()))
    (declare (type fixnum n nb-chunks nb-cores chunk-size))

    (flet ((worker (thread-id)
             (declare (ignorable thread-id))
             (let ((thread-sum 0.0d0))
               (declare (type double-float thread-sum))
               (loop
                 (let ((item (sb-concurrency:dequeue queue)))
                   (when (eq item :done)
                     (return thread-sum))
                   ;; task:
                   (let ((partial-sum 0.0d0))
                     (declare (type double-float partial-sum))
                     (loop with chunk-idx of-type fixnum = item
                           with chunk-start of-type fixnum = (* chunk-idx chunk-size)
                           with chunk-end of-type fixnum = (min (the fixnum (- n 1))
                                                                (the fixnum (- (+ chunk-start chunk-size) 1)))
                           for i of-type fixnum from chunk-start to chunk-end do
                             (incf partial-sum (/ (if (oddp i) -1.0d0 1.0d0) (the double-float (+ (* 2.0d0 i) 1.0d0)))))
                     (incf thread-sum partial-sum))))
               thread-sum)))
      
      ;; Start thread pools
      (dotimes (m nb-cores)
        (declare (type fixnum m))
        (let ((thread-id m))            ; capture
          (declare (type fixnum thread-id))
          (locally
              (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (push (sb-thread:make-thread (lambda () (worker thread-id))) workers))))

      ;; Enqueue tasks and finish by termination markers
      (dotimes (chunk-idx nb-chunks)
        (declare (type fixnum chunk-idx))
        (sb-concurrency:enqueue chunk-idx queue))
      (dotimes (m nb-cores)
        (declare (type fixnum m))
        (sb-concurrency:enqueue :done queue))

      ;; Wait for completion
      (let ((total-sum 0.0d0))
        (declare (type double-float total-sum))
        (dolist (w workers)
          (incf total-sum (the double-float (sb-thread:join-thread w))))
        (setq total-sum (* 4.0d0 total-sum))
        (locally
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          (let ((duration (/ (- (get-internal-real-time) start-time)
                             internal-time-units-per-second)))
            (format t "~%pi = ~a (in ~F s)~%" total-sum duration)
            duration))))))

(defun leibniz-3-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (leibniz-3)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
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

(defun leibniz-4 ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (sb-alien:load-shared-object "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/leibniz-cl/ffi-c-library/ffi.dll")
  
  (sb-alien:define-alien-routine
      ("leibniz_2" c-leibniz-2)
    sb-alien:double
    (n sb-alien:unsigned-long-long))

  (let ((duration (c-leibniz-2 +n+)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun leibniz-4-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (leibniz-4)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
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

(defun leibniz-5 ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (cffi:define-foreign-library libleibniz
    (:windows "c:/Users/noccis/Dropbox/local-repos/cl-speed-relative-performance/leibniz-cl/ffi-c-library/ffi.dll"))

  (cffi:use-foreign-library libleibniz)

  (cffi:defcfun ("leibniz_2" c-leibniz-2)
      :double
    (n :uint64))

  (let ((duration (c-leibniz-2 +n+)))
    (format t "C function returned duration = ~f~%" duration)
    duration))

(defun leibniz-5-benchmark-5-times ()
  ""
  (let ((nb-runs 5)
        (durations '()))
    (dotimes (i nb-runs)
      (let ((duration (leibniz-5)))
        (format t "Run ~D / ~D: ~F seconds~%" (1+ i) nb-runs duration)
        (push duration durations)))
    (setq durations (sort durations #'<))
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
  ""
  (format t "~%~%LEIBNIZ 1~%---------~%~%")
  (leibniz-1-benchmark-5-times)
  (format t "~%~%LEIBNIZ 2~%---------~%~%")
  (leibniz-2-benchmark-5-times)
  (format t "~%~%LEIBNIZ 3~%---------~%~%")
  (leibniz-3-benchmark-5-times)
  (format t "~%~%LEIBNIZ 4~%---------~%~%")
  (leibniz-4-benchmark-5-times)
  (format t "~%~%LEIBNIZ 5~%---------~%~%")
  (leibniz-5-benchmark-5-times))


;;; end
