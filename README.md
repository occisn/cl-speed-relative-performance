# cl-speed-relative-performance

Hobby project to compare Common Lisp (SBCL) execution speed with C.

In the below examples, we will volontarily not go into manual loop unrolling, SIMD, etc.

Each function is run 5 times, and the second best duration is kept.

Run on `gcc 13.2.0` and `sbcl 2.3.2`.

Several benchmarks:  
**[1. Leibniz formula](#1-leibniz-formula)**  
**[2. Morpho butterfly](#2-morpho-butterfly)**  
**[3. Naive Fibonacci](#3-naive-fibonacci)**  
**[4. Tight nested loops](#4-tight-nested-loops)**

Any comment? Open an [issue](https://github.com/occisn/repo/issues), or start a discussion [here](https://github.com/occisn/repo/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

## 1. Leibniz formula

As Niklas Heer dit it for its [speed comparison](https://github.com/niklas-heer/speed-comparison) of programming language, we will use [Leibniz formula](https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80) n times to calculate an approximate value of pi.

Actual digits of pi are: 3.14159265358979323846264338327950288419...

For n = 10,000,000,000

### Without parallelism:

SBCL speed is equivalent to C.

| Language            | Results                    | Execution duration | Function name |
|---------------------|----------------------------|--------------------|---------------|
| **C**, -O3          | **3.141592653**48834582099 | **9.7 s**          | leibniz_1     |
| **SBCL**, (speed 3) | **3.141592653**48834600000 | **10.0 s**         | leibniz-1     |

### With parallelism:

SBCL is twice slower than C.  
Obviously, speed is the same when C routine is called from SBCL with `ffi`.

| Language                         | Results                    | Execution duration | Function name |
|----------------------------------|----------------------------|--------------------|---------------|
| **C**, -O3 & openmg              | **3.141592653**48946803442 | **1.4 s**          | leibniz_2     |
| **SBCL** lparallel               | **3.141592653**488887      | **2.7 s**          | leibniz-2     |
| **SBCL** threads & queue         | **3.141592653**489321      | **2.7 s**          | leibniz-3     |
| **SBCL** calling C with sb-alien | **3.141592653**48946759033 | **1.4 s**          | leibniz-4     |
| **SBCL** calling C with CFFI     | **3.141592653**48946759033 | **1.4 s**           | leibniz-5     |

## 2. Morpho Butterfly

(later)

## 3. Naive Fibonacci

Computation of 46th Fibonacci method with naive (inefficient) method.

Inspired by Ben Dicken ([moving balls](https://benjdd.com/languages/), [Github](https://github.com/bddicken/languages)).

SBCL is 4 times slower than C.  
Obviously, speed is the same when C routine is called from SBCL with `ffi`.

| Language                                       | Execution duration | Function name     |
|------------------------------------------------|--------------------|-------------------|
| **C**, -O3                                     | **3.0 s**          | naive_fibonacci_1 |
| **C**, -O3, not inlined                        | **3.3 s**          | naive_fibonacci_2 |
| **SBCL**, (speed 3)                            | **11.9 s**         | naive-fibonacci-1 |
| **SBCL**, (speed 3), inlined with ceiling on n | **11.9 s**         | naive-fibonacci-2 |
| **SBCL** calling C with sb-alien               | **3.0 s**          | naive-fibonacci-A |
| **SBCL** calling C with CFFI                   | **3.0 s**          | naive-fibonacci-B |

## 4. Tight nested loops

Tight nested loops.

Inspired by Ben Dicken ([moving balls](https://benjdd.com/languages/), [Github](https://github.com/bddicken/languages)).

SBCL is 4 times slower than C.  
Obviously, speed is the same when C routine is called from SBCL with ffi.

| Language                            | Execution duration | Function name |
|-------------------------------------|--------------------|---------------|
| **C**, -O3, stack allocation        | **6.2 s**          | loops_1       |
| **C**, -O3, heap allocation         | **6.2 s**          | loops_2       |
| **SBCL** (speed 3)                  | **25.7 s**         | loops-1       |
| **SBCL** (speed 3), arrays on stack | **27.8 s**         | loops-2       |
| **SBCL** calling C with sb-alien    | **7.0 s**          | loops-A       |
| **SBCL** calling C with CFFI        | **6.9 s**          | loops-B       |

(end of README)
