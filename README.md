# Hypergeometrica

Hypergeometrica aims to democratize the calculation of pi to trillions of digits. As of March 2020, the software used to break world-record computations has remained closed source. This has been a 20+ year trend, and includes famous software such as y-cruncher, TachusPI, PiFast, and SuperPi. 

Please watch this [introductory video](https://www.youtube.com/watch?v=XanjZw5hPvE).

```
CL-USER> (asdf:load-package :hypergeometrica)
CL-USER> (in-package :hypergeometrica)
#<PACKAGE "HYPERGEOMETRICA">
HYPERGEOMETRICA> (partial-digits (binary-split (make-e-series) 0 100) 50)
271828182845904523536028747135266249775724709369996
```

## What is it?

Hypergeometrica is a Lisp library for performing extremely fast, extremely high-precision arithmetic. At the heart of it all are routines for doing fast multiplication. Hypergeometrica aims to support:

- Fast in-core multiplication using a variety of algorithms, from schoolbook to floating-point FFTs.

- Fast in-core multiplication for extremely huge numbers using exact convolutions via number-theoretic transforms. This is enabled by extremely fast 64-bit modular arithmetic.

- Fast out-of-core multiplication using derivatives of the original Cooley-Tukey algorithm.

On top of multiplication, one can build checkpointed algorithms for computing a variety of classical constants, like pi.


## How is it implemented?

It's a Lisp library that takes advantage of assembly code via SBCL's VOP facilities.

It would probably be easier to get higher performance quicker in C or C++, but there's a lot of non-hot-loop code (such as calculating suitable primes) that are better served without the baggage of a low-level language.


## What works and what doesn't?

There's a test suite, I recommend looking at that to see what (should be) working. In any case, a short list of features:

- Code to compute "suitable primes" for number-theoretic transforms.

- Basic bigint routines.

- An in-core number-theoretic transform employing many tricks for fast modular arithmetic.

- Binary-splitting for the computation of arbitrary hypergeometric series.

- In-core computation of pi, without any fancy algorithms for division, square root, or inversion.

An implementation of disk-backed bigints exists, but it's not vetted and I'm not sure it's a good architecture.

There's also a broken implementation of out-of-core multiplication called the "matrix Fourier algorithm" following Arndt. Some corner case isn't working, and I'm not even sure this is the best way to do out-of-core multiplication.

## Can I contribute?

Please, yes. Even if it's just telling me to document something. File an issue!

## I know a lot about {I/O, disks, computer arithmetic, assembly, SBCL, ...} but I'm not really interested in rolling up my sleeves for this project.

Please contact me so I have somebody to ask questions to!


## Where can I learn more about arbitrary precision arithmetic?

I'm keeping a [list of references](REFERENCES.md).
