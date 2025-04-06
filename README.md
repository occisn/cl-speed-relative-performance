# relative-performance

Hobby project to compare execution speed between several languages, namely C, Common Lisp, Emacs Lisp and... Excel.

**WORK IN PROGRESS - NOT STABILIZED YET**

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

``` C
int main(int argc, [[maybe_unused]] char* argv[argc+1]) {
  uint64_t n = 100000000; // 8 zeros
  double tmp = 0.0;
  double sign = 1.0;
  for (uint64_t i = 0; i <= n; i++) {
    tmp = tmp + sign / (2.0 * i + 1.0);
    sign = -sign;
  }
  tmp = 4 * tmp;
  printf("Result: %.20f\n", tmp);
  return EXIT_SUCCESS;
}
```

### Common Lisp

Dedicated file proposes 2 functions:

``` lisp
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
```

They yield 3.141592643589326d0 in several seconds.

### Emacs Lisp

Dedicated file proposes 2 functions:

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

With 10000000 (7 zeros only), they yield 3.1415925535897915 in several seconds.

### Excel

TODO

(end of README)
