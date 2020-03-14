# Hypergeometrica

Hypergeometrica aims to democratize the calculation of pi to trillions of digits. As of March 2020, the software used to break world-record computations has remained closed source. This has been a 20+ year trend, and includes famous software such as y-cruncher, TachusPI, PiFast, and SuperPi. 

Please watch this [introductory video](https://www.youtube.com/watch?v=XanjZw5hPvE).

## What is it?

Hypergeometrica is a Lisp library for performing extremely fast, extremely high-precision arithmetic. At the heart of it all are routines for doing fast multiplication. Hypergeometrica aims to support:

- Fast in-core multiplication using a variety of algorithms, from schoolbook to floating-point FFTs.

- Fast in-core multiplication for extremely huge numbers using exact convolutions via number-theotric transforms.

- Fast out-of-core multiplication using derivatives of the original Cooley-Tukey algorithm.

On top of multiplication, one can build checkpointed algorithms for computing a variety of classical constants, like pi.
