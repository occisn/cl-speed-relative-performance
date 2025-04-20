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
- Emacs Lisp calling C with FFI  
- Excel VBA  
- Excel formulas  
- Excel calling C


## Table of contents

**[Leibniz formula](#leibniz-formula)** : [synthesis](#synthesis), [C](#c), [Common Lisp SBCL](#common-lisp-sbcl), [Emacs Lisp](#emacs-lisp), [Excel](#excel)

## Leibniz formula

As Niklas Heer dit it for its [speed comparison](https://github.com/niklas-heer/speed-comparison) of programming language, we will use [Leibniz formula](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) n times to calculate an approximate value of pi.

### Synthesis

Actual digits of pi are: 3.14159265358979323846264338327950288419...

For n = 10,000,000,000 (10 zeros) with no SIMD or parallelization, Common Lisp SBCL is as quick as C:

| Language                                         | Results                        | Execution duration | Function name |
|--------------------------------------------------|--------------------------------|--------------------|---------------|
| **C**, -O3, basic                                | **3.141592653**68834583754     | 10.0 s             | leibniz 3     |
| **C**, -O3, with 4-loop unrolling                | **3.141592653**48834582099     | **10.0 s**         | leibniz 4     |
| **SBCL**, basic                                  | **3.14159265**258805040000 [3] | 177 s [3]          | leibniz 2     |
| **SBCL**, typed and (speed 3)                    | **3.141592653**68834600000     | 12.6 s             | leibniz 4     |
| **SBCL**, typed and (speed 3) + 4-loop unrolling | **3.141592653**48834600000     | **9.7 s** [4]      | leibniz 5     |
| **Emacs Lisp**, interpreted                      | **3.141592**55358979150330 [1] | 996 s [1]          | leibniz A 2   |
| **Emacs Lisp**, byte-compiled                    | **3.141592**55358979150330 [1] | 343 s [1]          | leibniz B 2   |
| **Emacs Lisp**, native-compiled                  | **3.141592**55358979150330 [1] | 332 s [1]          | leibniz C 2   |
| **Excel** VBA                                    | **3.14159265**458790000000 [3] | 300 s [3]          |               |
| **Excel** recursion (all cores)                  | **3.141592**75358981000000 [1] | 1300 s [1]         |               |
| **Excel** arrays formulas (all cores)            | **3.141592**55822236000000 [2] | 240 s [2]          |               |


[1] extrapolated from n = 10,000,000 (7 zeros)  
[2] extrapolated from n = 100,000,000 (8 zeros)  
[3] extrapolated from n = 1,000,000,000 (9 zeros)  
[4] 8- and 16-loop unrolling do not yield quicker results

### C

gcc version: 13.2.0

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

SBCL version: 2.4.10

Basic function is:

``` lisp
(defun leibniz ()
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

native compilation: libccjit.dll provided by msys2 version 3.5.7-4, containing gcc 13.20 (within msys64)

Basic function is:

``` elisp
(defun leibniz (n)
  "Calculate an approximation of π using Leibniz formula with N terms."
  (let ((tmp 0.0)
        (sign 1.0))
    (dotimes (i n)
      (setq tmp (+ tmp (/ sign (float (+ (* 2 i) 1)))))
      (setq sign (- sign)))
    (setq tmp (* 4 tmp))
    (message "Result: %.20f" tmp))
```

It is slightly accelerated by the use of `cl-evenp` function:
```elisp
(setq tmp (+ tmp (/ (if (cl-evenp i) 1.0 -1.0) (float (+ (* 2 i) 1)))))
```

Then by byte-compilation and native-compilation.


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
