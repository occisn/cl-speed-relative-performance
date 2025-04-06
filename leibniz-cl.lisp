
(defun leibniz-pi-1 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0d0)
        (sign 1.0d0))
    (dotimes (i n)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (* 4 tmp)))

(defun leibniz-pi-2 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0d0)
        (sign t))
    (dotimes (i n)
      (setq tmp (if sign
                   (+ tmp (/ 1.0d0 (+ (* 2 i) 1)))
                 (- tmp (/ 1.0d0 (+ (* 2 i) 1)))))
      (setq sign (not sign)))
    (* 4 tmp)))

;; (leibniz-pi-1 100000000) ; 8 zeros
;;    --> 3.141592643589326d0 in several seconds

;; (leibniz-pi-2 100000000) ; 8 zeros
;;    --> 3.141592643589326d0 in several seconds

;;; end
