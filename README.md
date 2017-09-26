# lc

[![Build Status](https://travis-ci.com/bsaleil/lc.svg?token=8gcbGkkhvfNySyut3swg&branch=master)](https://travis-ci.com/bsaleil/lc)

JIT compiler for Scheme targeting x86-64 platforms.

* Baptiste Saleil and Marc Feeley. Interprocedural Specialization of Higher-Order Dynamic Languages Without Static Analysis. In European Conference on Object-Oriented Programming (ECOOP'17), 2017.
<br/>[[pdf](http://drops.dagstuhl.de/opus/volltexte/2017/7271/pdf/LIPIcs-ECOOP-2017-23.pdf)]
* Baptiste Saleil and Marc Feeley. Type Check Removal Using Lazy Interprocedural Code Versioning. In Scheme and Functional Programming Workshop (SFPW'15), 2015. <br/>[[pdf](http://www.schemeworkshop.org/2015/sfpw4-2015-saleil-feeley.pdf)]
* Baptiste Saleil and Marc Feeley. Code Versioning and Extremely Lazy Compilation of Scheme. In Scheme and Functional Programming Workshop (SFPW'14), 2014. <br/>[[pdf](http://www.schemeworkshop.org/2014/papers/Saleil2014.pdf)]

### Building

A recent version of [Gambit](http://gambitscheme.org/) (probably >= 4.8.6) must be installed.

```bash
git clone https://github.com/bsaleil/lc.git
cd lc
make -j8
```

### Running

```bash
lc file.scm
```

### Example

To compute the 40th Fibonacci number:

#### fib.scm:
```
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(println (fib 40))
```

```bash
$ time ./lc fib.scm
165580141

real	0m0,550s
user	0m0,537s
sys	0m0,010s

```
