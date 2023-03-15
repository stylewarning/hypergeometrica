# Hypergeometrica

Hypergeometrica aims to democratize the calculation of pi to trillions of digits. As of March 2020, the software used to break world-record computations has remained closed source. This has been a 20+ year trend, and includes famous software such as y-cruncher, TachusPI, PiFast, and SuperPi. 

Please watch this [introductory video](https://www.youtube.com/watch?v=XanjZw5hPvE).

```
CL-USER> (asdf:load-system :hypergeometrica)
CL-USER> (in-package :hypergeometrica)
#<PACKAGE "HYPERGEOMETRICA">
HYPERGEOMETRICA> (compute-pi/ramanujan 100)
31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
HYPERGEOMETRICA> (compute-pi/chudnovsky 100)
31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
HYPERGEOMETRICA> (compute-e 100)
27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274
```

**Unfortunately**, Hypergeometrica cannot yet calculate pi in a completely *competent* way. What you see above does actually compute pi, but is taking a few very inefficient shortcuts. In order to be efficient, Hypergeometrica needs some additional key routines to eliminate these inefficient shortcuts.


## What is the most efficient way to calculate pi with Hypergeometrica?

The call `(MPD-PI b)` for $b$ bits of precision is the fastest way to compute pi with Hypergeometrica. Here is a call to calculate at least $b=100$ bits of pi.

```
CL-USER> (in-package :hypergeometrica)
HYPERGEOMETRICA> (mpd-pi 100)

terms = 4
[0 Δ0] split
[0 Δ0] sqrt
[4 Δ4] recip
[8 Δ4] final
#<MPD {10160AD883}>
```

This output can be interpreted as:

- `terms = 4` means 4 terms of the Chudnovsky series were calculated.
- `[x Δy] thing` means `x` milliseconds have elapsed since the start of the computation, and `thing` took `y` milliseconds since the last step
- `#<MPD {...}>` is the object returned. Currently there is no base conversion routine to actually show this. One can use `mpd-mpfr` to convert it into an MPFR object for viewing.

The function `MPD-PI` can be tested for $b=10^0$, $b=10^1$, and so on to $b=10^{n-1}$ against MPFR via the call `(TEST-PI n)`.

```
HYPERGEOMETRICA> (test-pi 5)

terms = 2
[0 Δ0] split
[0 Δ0] sqrt
[0 Δ0] recip
[0 Δ0] final

1 bits [0 digits]
==> error = -3

terms = 2
[0 Δ0] split
[0 Δ0] sqrt
[0 Δ0] recip
[0 Δ0] final

10 bits [3 digits]
==> error = 0

terms = 4
[0 Δ0] split
[4 Δ4] sqrt
[4 Δ0] recip
[8 Δ4] final

100 bits [30 digits]
==> error = 0

terms = 23
[0 Δ0] split
[156 Δ156] sqrt
[228 Δ72] recip
[284 Δ56] final

1000 bits [301 digits]
==> error = 0

terms = 214
[28 Δ28] split
[2124 Δ2096] sqrt
[4296 Δ2172] recip
[5884 Δ1588] final

10000 bits [3010 digits]
==> error = 0
```

The output is reasonably similar to the last function, except the `error = b` indicates there were `b` bits of difference between the Hypergeometrica's value and MPFR's value.


## What is it?

Hypergeometrica is a Lisp library for performing extremely fast, extremely high-precision arithmetic. At the heart of it all are routines for doing fast multiplication. Hypergeometrica aims to support:

- Fast in-core multiplication using a variety of algorithms, from schoolbook to floating-point FFTs.

- Fast in-core multiplication for extremely huge numbers using exact convolutions via number-theoretic transforms. This is enabled by extremely fast 64-bit modular arithmetic.

- Fast out-of-core multiplication using derivatives of the original Cooley-Tukey algorithm.

- Implementation of dyadic rationals for arbitrary precision float-like numbers.

- Elementary automatic parallelization when reasonable.

On top of multiplication, one can build checkpointed algorithms for computing a variety of classical constants, like pi.


## How is it implemented?

It's a Lisp library that takes advantage of assembly code via SBCL's VOP facilities.

It would probably be easier to get higher performance quicker in C or C++, but there's a lot of non-hot-loop code (such as calculating suitable primes) that are better served without the baggage of a low-level language.


## What works and what doesn't?

There's a test suite, I recommend looking at that to see what (should be) working. In any case, a short list of features:

- Basic bigint (`MPZ`) routines.

- Basic dyadic rational (`MPD`) routines.

- Code to compute "suitable primes" for number-theoretic transforms.

- An in-core number-theoretic transform employing tricks for fast modular arithmetic.

- Binary-splitting for the computation of arbitrary hypergeometric series.

- Out-of-core/disk-based number representation and automatic upgrading, with specialized algorithms.

- In-core computation of pi, with basic asymptotically fast algorithms for division, square root, or inversion.


An implementation of disk-backed bigints exists, but it's not vetted and I'm not sure it's a good architecture.

There's also a broken implementation of out-of-core multiplication called the "matrix Fourier algorithm" following Arndt. Some corner case isn't working, and I'm not even sure this is the best way to do out-of-core multiplication.


## Can I contribute?

Please, yes. Even if it's just telling me to document something. File an issue!


## I know a lot about {I/O, disks, computer arithmetic, assembly, SBCL, ...} but I'm not really interested in rolling up my sleeves for this project.

Please contact me so I have somebody to ask questions to!


## Where can I learn more about arbitrary precision arithmetic?

I'm keeping a [list of references](REFERENCES.md).
