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

**[Leibniz formula](#leibniz-formula)** : [synthesis](#synthesis), [C](#c), [Common Lisp SBCL](#common-lisp-sbcl), [Emacs Lisp](#emacs-lisp), [Excel](#excel)

## Leibniz formula

As Niklas Heer dit it for its [speed comparison](https://github.com/niklas-heer/speed-comparison) of programming language, we will use [Leibniz formula](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) n times to calculate an approximate value of pi.

### Synthesis

Actual digits of pi are: 3.14159265358979323846264338327950288419...

For n = 10000000000 (10 zeros) with no SIMD or parallelization, Common Lisp SBCL is as quick as C:

| Language                                         | Results                    | Execution duration | Function name |
|--------------------------------------------------|----------------------------|--------------------|---------------|
| **C**, -O3, basic                                | **3.141592653**68834583754 | 10.0 s             | leibniz 3     |
| **C**, -O3, with 4-loop unrolling                | **3.141592653**48834582099 | 10.0 s             | leibniz 4     |
| **SBCL**, basic                                  |                            | 177 s [3]          | leibniz 2     |
| **SBCL**, typed and (speed 3)                    | **3.141592653**68834600000 | 12.6 s             | leibniz 4     |
| **SBCL**, typed and (speed 3) + 4-loop unrolling | **3.141592653**48834600000 | 9.7 s [4]          | leibniz 5     |
| **Emacs Lisp**, interpreted                      |                            | 689 s [1]          |               |
| **Emacs Lisp**, byte-compiled                    |                            | 428 s [1]          |               |
| **Emacs Lisp**, native-compiled                  |                            | 415 s [1]          |               |
| **Excel** VBA                                    |                            | 300 s [3]          |               |
| **Excel*** recursion (all cores)                 |                            | 1300 s [1]         |               |
| **Excel** arrays formulas (all cores)            |                            | 240 s [2]          |               |


[1] extrapolated from n = 10000000 (7 zeros)  
[2] extrapolated from n = 100000000 (8 zeros)  
[3] extrapolated from n = 1000000000 (9 zeros)  
[4] 8- and 16-loop unrolling do not yield quicker results

### C

Basic function is:
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

### Common Lisp SBCL

Basic function is:

``` lisp
(defun leibniz-2 ()
  "Calculate an approximation of π using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign 1.0d0))
    (dotimes (i *n8*)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (* 4 tmp)))
```

Several optimizations are proposed, including type declaration and full use of SBCL compiler.

### Emacs Lisp

Basic function is:

``` elisp
(defun leibniz-pi-1 (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign 1.0))
    (dotimes (i n (* 4 tmp))
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))))
```

### Excel

**File A** = **VBA**

``` VBA
Function Leibniz(n As Long) As Double
    Dim tmp As Double
    Dim sign As Double
    Dim i As Long
    tmp = 0
    sign = 1
    For i = 0 To n
        tmp = tmp + sign / (2 * i + 1)
        sign = -sign
    Next i
    Leibniz = 4 * tmp
End Function
```

**File B** = 1000x1000 table in spreadsheet  
only 1000000 (6 zeros) but already 16 Mo

**File C** = **recursion**

**File D** = **array formulas**

(end of README)
