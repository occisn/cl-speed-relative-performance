# relative-performance

Hobby project to compare execution speed between several languages, namely C, Common Lisp, Emacs Lisp and... Excel.

**WORK IN PROGRESS**

One of the objectives is to see to which extent Common Lisp could be as fast as C.

We will first use Leibniz formula as a test case.

More precisely, compared languages will be:  
- C: single-threaded, with parallelism  
- Common Lisp: single-threaded, with parallelism, calling C with CFFI  
- Emacs Lisp: interpreted, byte-compiled, native-compiled, calling C with FFI  
- Excel: VBA, recursion, array formulas, calling C  
- (for fun) perhaps Gnu Calc

## Table of contents

**[Leibniz formula](#leibniz-formula)** :  
[Synthesis](#synthesis)  
[C](#c)  
[Common Lisp SBCL](#common-lisp-sbcl)  
[Emacs Lisp](#emacs-lisp)  
Excel : [VBA](#excel-by-vba), [spreadsheet](#excel-by-spreadsheet), [recursion](#excel-by-recursion), [array formulas](#excel-by-array-formulas)

## Leibniz formula

As Niklas Heer dit it for its [speed comparison](https://github.com/niklas-heer/speed-comparison) of programming language, we will use [Leibniz formula](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) n times to calculate an approximate value of pi.

### Synthesis

Actual digits of pi are: 3.14159265358979323846264338327950288419...

For n = 10,000,000,000 (10 zeros) with no SIMD or parallelization at this stage, **Common Lisp SBCL is as quick as C**:

| Language                                         | Results                        | Execution duration | Function name |
|--------------------------------------------------|--------------------------------|--------------------|---------------|
| **C**, -O3, basic                                | **3.141592653**68834583754     | 10.0 s             | leibniz 3     |
| **C**, -O3, with 4-loop unrolling                | **3.141592653**48834582099     | **10.0 s**         | leibniz 4     |
| **C** with parallelism                           | ???                            | ???                |               |
| **SBCL**, basic                                  | **3.14159265**258805040000 [3] | 177 s [3]          | leibniz 2     |
| **SBCL**, typed and (speed 3)                    | **3.141592653**68834600000     | 10.0 s             | leibniz 5     |
| **SBCL**, typed and (speed 3) + 4-loop unrolling | **3.141592653**48834600000     | **9.7 s**          | leibniz 6     |
| **SBCL** with parallelism                        | ???                            | ???                |               |
| **SBCL** calling C                               | ???                            | ???                |               |
| **Emacs Lisp**, interpreted                      | **3.141592**55358979150330 [1] | 9960 s [1]         | leibniz A 2   |
| **Emacs Lisp**, byte-compiled                    | **3.141592**55358979150330 [1] | 3430 s [1]         | leibniz B 2   |
| **Emacs Lisp**, native-compiled                  | **3.141592**55358979150330 [1] | 3320 s [1]         | leibniz C 2   |
| **Emacs Lisp** calling C                         | ???                            | ???                |               |
| **Excel** VBA                                    | **3,1415926**4457277000000 [3] | 125 s [3]          | VBA 3         |
| **Excel** recursion (all cores)                  | **3.1415926**3880205000000 [2] | 1043 s [2]         | recursion 2   |
| **Excel** arrays formulas (all cores)            | **3.141592**55822236000000 [2] | 2464 s [2]         |               |
| **Excel** calling C                              | ???                            | ???                |               |

[1] extrapolated from n = 10,000,000 (7 zeros)  
[2] extrapolated from n = 100,000,000 (8 zeros)  
[3] extrapolated from n = 1,000,000,000 (9 zeros)  

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
  "Calculate an approximation of pi using Leibniz formula."
  (let ((tmp 0.0d0)
        (sign 1.0d0))
    (dotimes (i *n8*)
      (setq tmp (+ tmp (/ sign (+ (* 2 i) 1))))
      (setq sign (- sign)))
    (* 4 tmp)))
```

It is accelerated within leibniz-5 function by type declarations, (speed 3) and the use of `evenp`.

leibniz-5-bis: play on integer calculation, but not quicker

leibniz-6: introduction of 4-loop unrolling

8- and 16-loop unrolling do not yield quicker results

### Emacs Lisp

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

Native compilation: libccjit.dll provided by msys2 version 3.5.7-4, containing gcc 13.20 (within msys64)

### Excel by VBA

Basic function is in VBA 1 file:
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

It can be accelerated by almost a factor 2 by grouping terms (see VBA 2):
``` VBA
Function Leibniz(n As Long) As Double
    Dim tmp As Double
    Dim i As Long
    tmp = 0
    For i = 0 To (n - 1) Step 2
        tmp = tmp + 1 / (2 * i + 1) - 1 / (2 * i + 3)
    Next i
    Leibniz = 4 * tmp
End Function
```

Let's use # to indicate Double literals, avoiding implicit type conversions. See VBA 3:
``` VBA
Function Leibniz(n As Long) As Double
    Dim tmp As Double
    Dim i As Long
    tmp = 0#
    For i = 0 To (n - 1) Step 2
        tmp = tmp + 1# / (2# * i + 1#) - 1 / (2# * i + 3#)
    Next i
    Leibniz = 4# * tmp
End Function
```

In VBA 4, we try to divide 1-1000000000 (9 zeros) range into chunks, but this does not increase speed (no parallelism in such situation).

### Excel by spreadsheet

File B proposes a 100 x 100 = 10,000 (4 zeros) table in a spreadsheet.

The same file with a 1,000 x 1,000 = 1,000,000 (6 zeros) table would weight around 16 Mo.

### Excel by recursion

File C, version 1, divises 0 - 10,000,000 (7 zeros) in 3054 chunks of 3275 range (Excel recursion maximum depth with 2 parameters), then uses recursion on each chunk.

Typical formula is:
``` Excel
=LET(
SUB;LAMBDA(ME;A;B;SI(B<A;0;(-1)^B/(2*B+1)+ME(ME;A;B-1)));
SUB(SUB;B10;C10))
```

Version 2 performs the same, but chunks are 32 times larger, since each formula computes 32 terms of the sum:
``` Excel
=LET(
SUB;LAMBDA(ME;A;B;SI(A+31>B;0;1/(2*A+1)-1/(2*A+3)+1/(2*A+5)-1/(2*A+7)+1/(2*A+9)-1/(2*A+11)+1/(2*A+13)-1/(2*A+15)+1/(2*A+17)-1/(2*A+19)+1/(2*A+21)-1/(2*A+23)+1/(2*A+25)-1/(2*A+27)+1/(2*A+29)-1/(2*A+31)+1/(2*A+33)-1/(2*A+35)+1/(2*A+37)-1/(2*A+39)+1/(2*A+41)-1/(2*A+43)+1/(2*A+45)-1/(2*A+47)+1/(2*A+49)-1/(2*A+51)+1/(2*A+53)-1/(2*A+55)+1/(2*A+57)-1/(2*A+59)+1/(2*A+61)-1/(2*A+63)+ME(ME;A+32;B)));
SUB(SUB;B10;C10))
```
So, only ~ 30 chunks are needed.  
Actually ~ 300, since the increase of speed allows calculating for n = 100,000,000 (8 zeros).

If we hard-code the chunk limits, there is apparently a small increase of speed, but nothing significant.

### Excel by array formulas

File D, version 1, divises 0 - 100,000,000 (8 zeros) range in 96 chunks of 1,048,576 range (Excel sequence max size), then uses array formulas on each chunk.

Version 2 performs the same, but chunks limits are hard-coded: it does *not* improve speed.

(end of README)
