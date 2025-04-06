(defun leibniz-pi-A-1 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign 1.0))
    (dotimes (i n (* 4 tmp))
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))))

(defun leibniz-pi-A-2 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign t))
    (dotimes (i n (* 4 tmp))
      (setq tmp (if sign
                   (+ tmp (/ 1 (float (+ (* 2 i) 1))))
                 (- tmp (/ 1 (float (+ (* 2 i) 1))))))
      (setq sign (not sign)))))

;; (leibniz-pi-A-1 10000000) ; 7 zeros
;;    --> 3.1415925535897915 in several seconds

;; (leibniz-pi-A-2 10000000) ; 7 zeros
;;    --> 3.1415925535897915 in several seconds

;;; end
