# lc

[![Build Status](https://travis-ci.com/bsaleil/lc.svg?token=8gcbGkkhvfNySyut3swg&branch=master)](https://travis-ci.com/bsaleil/lc)

JIT compiler for Scheme targeting Linux x86-64 platforms.

* Baptiste Saleil and Marc Feeley. Interprocedural Specialization of Higher-Order Dynamic Languages Without Static Analysis. In European Conference on Object-Oriented Programming (ECOOP'17), 2017.
<br/>[[pdf](http://drops.dagstuhl.de/opus/volltexte/2017/7271/pdf/LIPIcs-ECOOP-2017-23.pdf)]
* Baptiste Saleil and Marc Feeley. Type Check Removal Using Lazy Interprocedural Code Versioning. In Scheme and Functional Programming Workshop (SFPW'15), 2015. <br/>[[pdf](http://www.schemeworkshop.org/2015/sfpw4-2015-saleil-feeley.pdf)]
* Baptiste Saleil and Marc Feeley. Code Versioning and Extremely Lazy Compilation of Scheme. In Scheme and Functional Programming Workshop (SFPW'14), 2014. <br/>[[pdf](http://www.schemeworkshop.org/2014/papers/Saleil2014.pdf)]

### Building

LC depends on a modified version of the Gambit Scheme compiler that must be installed before building LC:

```bash
# Build the modified version of Gambit
git clone https://github.com/bsaleil/gambit
cd gambit
mkdir build
./configure --enable-single-host --prefix=$(pwd)/build
make -j8
make install
```

The ```gsc``` executable of the modified version of Gambit must be available in ```PATH``` both when building and running LC:

```bash
# Make 'gsc' binary available in PATH
export PATH=$(pwd)/build/bin:$PATH
```

Then, LC can be built:

```bash
# Build LC
git clone https://github.com/bsaleil/lc -b stable
cd lc
make -j8
```

### Running

Make sure the ```gsc``` binary of the modified version of gambit is also available in ```PATH``` when running lc.

```bash
./lc file.scm
```

### Example

To compute the 40th Fibonacci number:

#### fib.scm:
```scheme
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
