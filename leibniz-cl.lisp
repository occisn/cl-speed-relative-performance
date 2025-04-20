
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

 
;;; end
