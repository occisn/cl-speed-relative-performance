
(ql:quickload :lparallel)

(defparameter *n6* 1000000)      ;  6 zeros
(defparameter *n7* 10000000)     ;  7 zeros
(defparameter *n8* 100000000)    ;  8 zeros
(defparameter *n9* 1000000000)   ;  9 zeros
(defparameter *n10* 10000000000) ; 10 zeros
(declaim (type fixnum *n7* *n8* *n9* *n10*))

;;; change of flag:
(defun leibniz-pi-1 (&optional (n *n8*))
  "Calculate an approximation of π using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign t))
    (dotimes (i n)
      (setq tmp (if sign
                    (+ tmp (/ 1.0d0 (+ (* 2 i) 1)))
                    (- tmp (/ 1.0d0 (+ (* 2 i) 1)))))
      (setq sign (not sign)))
    (* 4 tmp)))

;;; change of sign:
(defun leibniz-2 (&optional (n *n9*))
  "Calculate an approximation of π using Leibniz formula."
  (let ((start-time (get-internal-real-time))
        (tmp 0.0d0)
        (sign 1.0d0))
    (dotimes (i n)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (setq tmp (* 4 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (format t "Result: ~,20f in ~f seconds~%" tmp duration)))

;;; change of sign + type, with no speed 3:
  (defun leibniz-3 (&optional (n *n8*))
    "Calculate an approximation of π using Leibniz formula."
    (declare (type fixnum n)
             (optimize (speed 0) (safety 3) (debug 0)))
    (let ((tmp 0.0d0)
          (sign 1))
      (declare (type double-float tmp)
               (type fixnum sign))
      (dotimes (i n)
        (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
        (setq sign (- sign)))
      (* 4 tmp))))

;;; change of sign + type and speed 3:
(defun leibniz-4 (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (tmp 0.0d0)
        (sign 1)
        (i 0))
    (declare (type double-float tmp)
             (type fixnum sign i))
    (loop while (<= i n)
          do (progn
               (setq tmp (+ tmp (/ sign (+ (* 2.0d0 i) 1.0d0)))
                     sign (- sign)
                     i (+ i 1))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; evenp + type and speed 3:
(defun leibniz-5 (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (tmp 0.0d0)
        (i 0))
    (declare (type double-float tmp)
             (type fixnum i))
    (loop while (<= i n)
          do (progn
               (if (evenp i)
                   (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0))))
                   (setq tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0)))))
               (setq i (+ i 1))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;; attempt of small optimization with integer calculations, but not quicker
(defun leibniz-5-bis (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (tmp 0.0d0)
        (i 0)
        (i21 1))                        ; i*2+1
    (declare (type double-float tmp)
             (type fixnum i i21))
    (loop while (<= i n)
          do (progn
               (if (evenp i)
                   (setq tmp (+ tmp (/ 1.0d0 i21)))
                   (setq tmp (- tmp (/ 1.0d0 i21))))
               (setq i (+ i 1)
                     i21 (+ i21 2))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; 2-loop unrolling to avoid explicite change of sign + type and speed 3:
(defun leibniz-5ter (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (nmax (- n 1))
        (tmp 0.0d0)
        (i 0))
    (declare (type double-float tmp)
             (type fixnum i nmax))
    (loop while (<= i nmax)
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0))))
               (setq tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 i) 3.0d0))))
               (setq i (+ i 2))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; 4-loop unrolling:
(defun leibniz-6 (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (nmax (- n 3))
        (tmp 0.0d0)
        (i 0))
    (declare (type fixnum i nmax)
             (type double-float tmp))
    (loop while (<= i nmax)
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 1 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 2 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 3 i)) 1.0d0)))
                     i (+ i 4))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; 8-loop unrolling:
(defun leibniz-7 (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (nmax (- n 7))
        (tmp 0.0d0)
        (i 0))
    (declare (type fixnum i nmax)
             (type double-float tmp))
    (loop while (<= i nmax)
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 1 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 2 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 3 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 4 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 5 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 6 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 7 i)) 1.0d0)))
                     i (+ i 8))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;;; 16-loop unrolling:
(defun leibniz-8 (&optional (n *n10*))
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n))
  (let ((start-time (get-internal-real-time))
        (nmax (- n 15))
        (tmp 0.0d0)
        (i 0))
    (declare (type fixnum nmax i)
             (type double-float tmp))
    (loop while (<= i nmax)
          do (progn
               (setq tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 i) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 1 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 2 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 3 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 4 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 5 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 6 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 7 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 8 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 9 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 10 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 11 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 12 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 13 i)) 1.0d0)))
                     tmp (+ tmp (/ 1.0d0 (+ (* 2.0d0 (+ 14 i)) 1.0d0)))
                     tmp (- tmp (/ 1.0d0 (+ (* 2.0d0 (+ 15 i)) 1.0d0)))
                     i (+ i 16))))
    (setq tmp (* 4.0d0 tmp))
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" tmp duration))))
  nil)

;; lparallel

(defun leibniz-9 ()
  ""

  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (let* ((start-time (get-internal-real-time))
         (nmax (floor (1- *n10*) 2))
         (nb-chunks 30)
         (chunk-size (floor (- nmax 0) nb-chunks))
         (starts (make-array nb-chunks :element-type 'fixnum :initial-contents
                             (loop for j of-type fixnum from 1 to nb-chunks
                                   for start of-type fixnum = 0 then (+ start chunk-size)
                                   collect start))))

    (declare (type fixnum nmax nb-chunks chunk-size)
             (type (simple-array fixnum (*)) starts))

    (setq lparallel:*kernel* (lparallel:make-kernel 8))

    (let* ((partial-sums
             (lparallel:pmap
              '(simple-array double-float (*))
              (lambda (start)
                (declare (type fixnum start))
                (let ((end (min nmax (+ start chunk-size)))
                      (sum1 0.0d0))
                  (declare (type fixnum end)
                           (type double-float sum1))
                  (loop for i of-type fixnum from start below end
                        do
                           (progn
                             (setq sum1 (+ sum1 (/ 1.0d0 (+ (* 2.0d0 (* 2.0d0 i)) 1.0d0))))
                             (setq sum1 (- sum1 (/ 1.0d0 (+ (* 2.0d0 (+ (* 2.0d0 i) 1)) 1.0d0))))))
                  sum1))
              starts))
           (res (* 4.0d0 (reduce #'+ partial-sums)))
           (end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (declare (type vector partial-sums)
               (type double-float res))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" res duration)))))

;; SBCL threads

(defun leibniz-10 ()
  ""

  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (let* ((start-time (get-internal-real-time))
         (nmax (floor (1- *n10*) 2))
         (nb-chunks 50)
         (chunk-size (floor (- nmax 0) nb-chunks))
         (mutex (sb-thread:make-mutex :name "counter-mutex"))
         (res 0.0d0)
         (threads (loop for i from 0 below nb-chunks
                        collect (sb-thread:make-thread
	                         (lambda (i)
                                   (declare (type fixnum i))
                                   (let* ((start (* i chunk-size))
                                          (end (min nmax (+ start chunk-size)))
                                          (sum1 0.0d0))
                                     (declare (type fixnum start end)
                                              (type double-float sum1))
                                     (loop for j of-type fixnum from start below end
                                           do
                                              (progn
                                                (setq sum1 (+ sum1 (/ 1.0d0 (+ (* 2.0d0 (* 2.0d0 j)) 1.0d0))))
                                                (setq sum1 (- sum1 (/ 1.0d0 (+ (* 2.0d0 (+ (* 2.0d0 j) 1)) 1.0d0))))))
                                     (sb-thread:with-mutex (mutex)
                                       (setq res (+ res sum1)))
                                     nil))
                                 :arguments (list i)))))
    (declare (type fixnum nmax nb-chunks chunk-size)
             (type double-float res))
    
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    (setq res (* 4.0d0 res))
    
    (let* ((end-time (get-internal-real-time))
           (duration (/ (- end-time start-time) (float internal-time-units-per-second))))
      (locally
          (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (format t "Result: ~,20f in ~f seconds~%" res duration)))))

;;; end



