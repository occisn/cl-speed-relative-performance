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

TODO:

| Language        | Results                | Execution duration |
|-----------------|------------------------|--------------------|
| C               | 3.14159266358932587337 |                    |
| Common Lisp     | 3.141592643589326      |                    |
| Emacs Lisp      |                        |                    |
| Excel VBA       | 3.14159266358937000000 |                    |
| Excel recursion | 3.14159266358962000000 |                    |
| Excel arrays    | 3.14159266358979000000 |                    |


### C

The basis function `leibniz_3` is:
``` C
int leibniz_3() {
  uint64_t n9 = 1000000000; // 9 zeros
  double tmp = 0.0;
  double sign = 1.0;
  for (uint64_t i = 0; i <= n9; i++) {
    tmp = tmp + sign / (2.0 * i + 1.0);
    sign = -sign;
  }
  tmp = 4 * tmp;
  printf("Result: %.20f\n", tmp);
  return EXIT_SUCCESS;
}
```

Several optimizations are proposed, all with -O3 flag:


`leibniz_1`: calculate (-1)^i and is very slow  
`leibniz_2`: work with a change of boolean flag = 1 second  
`leibniz_3`: change of sign = 1 second  
`leibniz_4`: loop unrolling with 4 operations per iteration = 1 second = no gain  
`leibniz_5`: parallelization with 4 operations per iteration = 0.28 second  
`leibniz_6`: parallelization with 16 operations per iteration = 0.28 second  
`leibniz_7`: SIMD vectorization with 8-array for float precision, but no parallelization = 0.7 second  
`leibniz_8`: SIMD vectorization with 4-array for double precision, but no parallelization = 0.5 second  
`leibniz_9`: SIMD vectorization with 8-array for float precision, and parallelization = 0.20 second  
`leibniz_10`: SIMD vectorization with 4-array for double precision, and parallelization = 0.14 second

There is a x7 gain between initial `leibniz_3` and final `leibniz_10` optimized with  SIMD vectorization with 4-array for double precision, and parallelization.

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

**File A** = **VBA**

``` VBA
Function ReversedNumber(ByVal n As Long) As Long
    Dim LastDigit As Long
    ReversedNumber = 0
    Do While n > 0
        LastDigit = n Mod 10
        n = n \ 10
        ReversedNumber = 10 * ReversedNumber + LastDigit
    Loop
End Function
```

Yields: 3.14159266358937000000

**File B** = 1000x1000 table in spreadsheet  
only 1000000 (6 zeros) but already 16 Mo

Yields 3.14159365258979000000

**File C** = **recursion**

Yields 3.14159266358962000000 

**File D** = **array formulas**

Yields 3.14159266358979000000 

(end of README)
