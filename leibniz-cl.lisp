
(defparameter *n7* 10000000)   ; 7 zeros
(defparameter *n8* 100000000)  ; 8 zeros
(defparameter *n9* 1000000000) ; 9 zeros
(declaim (type fixnum *n7* *n8* *n9*))

;;; change of flag:
(defun leibniz-pi-1 ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign t))
    (dotimes (i *n8*)
      (setq tmp (if sign
                    (+ tmp (/ 1.0d0 (+ (* 2 i) 1)))
                    (- tmp (/ 1.0d0 (+ (* 2 i) 1)))))
      (setq sign (not sign)))
    (* 4 tmp)))

;;; change of sign:
(defun leibniz-2 ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign 1.0d0))
    (dotimes (i *n8*)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (* 4 tmp)))

;;; change of sign + type, with no speed 3:
(defun leibniz-3 ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign 1))
    (declare (type double-float tmp)
             (type fixnum sign))
    (dotimes (i *n8*)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (* 4 tmp)))

;;; change of sign + type and speed 3:
(defun leibniz-4 ()
  "Calculate an approximation of π using Leibniz formula."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((tmp 0.0d0)
        (sign 1)
        (i 0))
    (declare (type double-float tmp)
             (type fixnum sign i))
    (loop while (<= i *n8*)
          do (progn
               (setq tmp (+ tmp (/ sign (+ (* 2.0d0 i) 1.0d0)))
                     sign (- sign)
                     i (+ i 1))))
    (setq tmp (* 4.0d0 tmp))
    (locally
        (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (format t "Result: ~,20f~%" tmp)))
  nil)


;; (leibniz-1) ; with 8 zeros
;;    --> 3.141592643589326d0 in several seconds

;; (leibniz-2) ; with 8 zeros
;;    --> 3.141592643589326d0 in 3 seconds (unplugged)

;; (leibniz-3) ; with 8 zeros
;;    --> 3.141592643589326d0 in 23 seconds (unplugged)
;; type declaration slow down things if no (speed 3)

;; (leibniz-4) is very quick
;; 8 zeros --> 0.2 s
;; 9 zeros --> 2 s
;; (unplugged)

;;; end
