;;; -*- lexical-binding: t; -*-

(defun leibniz-C-1 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((start-time (current-time))
        (tmp 0.0)
        (sign 1.0))
    (dotimes (i n)
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))
    (setq tmp (* 4 tmp))
    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time start-time))))
      (message "Result: %.20f in %f seconds" tmp duration))))

(defun leibniz-C-2 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((start-time (current-time))
        (tmp 0.0))
    (dotimes (i n)
      (setq tmp (+ tmp (/ (if (cl-evenp i) 1.0 -1.0) (float (+ (* 2 i) 1))))))
    (setq tmp (* 4 tmp))
    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time start-time))))
      (message "Result: %.20f in %f seconds" tmp duration))))

(defun leibniz-C-3 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((start-time (current-time))
        (tmp 0.0)
        (i 0))
    (while (<= i (- n 1))
      (setq tmp (+ tmp (/ 1.0 (float (+ (* 2 i) 1))))
            tmp (- tmp (/ 1.0 (float (+ (* 2 i) 3))))
            i (+ i 2)))
    (setq tmp (* 4 tmp))
    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time start-time))))
      (message "Result: %.20f in %f seconds" tmp duration))))

;; (leibniz-C-1 10000000) ; 7 zeros
;; (leibniz-C-2 10000000) ; 7 zeros
;; (leibniz-C-3 10000000) ; 7 zeros

;;; end
