;;; -*- lexical-binding: t; -*-

(defun leibniz-pi-A (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((start-time (current-time))
        (tmp 0.0)
        (sign 1.0))
    (dotimes (i n (* 4 tmp))
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))
    (setq tmp (* 4 tmp))
    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time start-time))))
      (message "Result: %.20f in %f seconds" tmp duration))))

;; (leibniz-pi-A 10000000) ; 7 zeros

;;; end
