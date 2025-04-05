# relative-performance

Hobby project to compare execution speed between several languages, namely C, Common Lisp, Emacs Lisp and... Excel.

One of the objectives is to see to which extent Common Lisp could be as fast as C.

We will first use Leibniz formula as a test case.

More precisely, compared languages will be:  
- C  
- C with parallelism  
- Common Lisp  
- Common Lisp with parallelism  
- Common Lisp calling C with CFFI  
- Emacs Lisp  
- Emacs Lisp with native compilation  
- Excel VBA  
- Excel formulas.


## Table of contents

**[Leibniz formula](#leibniz-formula)** : [synthesis](#synthesis), [C](#c), [Common Lisp](#common-lisp), [Emacs Lisp](#emacs-lisp), [Excel](#excel)

## Leibniz formula

As Niklas Heer dit it for its [speed comparison](https://github.com/niklas-heer/speed-comparison) of programming language, we will use [Leibniz formula](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) 100000000 (8 zeros) times to calculate an approximate value of pi.

### Synthesis

TODO

### C

TODO

### Common Lisp

TODO

### Emacs Lisp

Dedicates file proposes 2 functions:

``` elisp
(defun leibniz-pi-1 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign 1.0))
    (dotimes (i n (* 4 tmp))
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))))

(defun leibniz-pi-2 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign t))
    (dotimes (i n (* 4 tmp))
      (setq tmp (if sign
                   (+ tmp (/ 1 (float (+ (* 2 i) 1))))
                 (- tmp (/ 1 (float (+ (* 2 i) 1))))))
      (setq sign (not sign)))))
```

They yield 3.1415925535897915 in several seconds.

### Excel

TODO

(end of README)
