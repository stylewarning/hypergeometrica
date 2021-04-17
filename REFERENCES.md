# References

## "The Computation of Classical Constants" by the Chudnovsky Bros

This is a _classic_ paper introducing Chudnovsky's algorithm for
computing pi and its efficient computation. I consider it landmark.

- [Link](https://pdfs.semanticscholar.org/51ac/805b2cd8cb287c47aa04442e414e24c0881b.pdf)

Some more helpful materials:

- [Tito Piezas articles](https://sites.google.com/site/tpiezas/0027)

- [Ramanujan's Series for 1/pi](https://faculty.math.illinois.edu/~berndt/articles/monthly567-587.pdf)

- [A Detailed Proof of Chudnovsky Formula](https://arxiv.org/abs/1809.00533): Excellent first-principles presentation of the formula.


## "Matters Computational" (aka fxtbook)

Joerg Arndt's tour de force on computer arithmetic algorithms. He's
been in the business for a while.

- [Link](https://www.jjj.de/fxt/)

## Binary Splitting

There are a few references on binary splitting. The canonical one for
me is the Haible paper.

- [Haible & Papanikolaou](https://www.ginac.de/CLN/binsplit.pdf)

- [Gourdon & Sebah](http://numbers.computation.free.fr/Constants/Algorithms/splitting.ps)


## Number-Theoretic Transforms

- [Link](https://pdfs.semanticscholar.org/c48a/2408d3ff16836935275bab16947fefc00f1a.pdf): "Faster arithmetic for number-theoretic transforms" by David Harvey. These slides are generally useful for the implementation of NTTs. Includes Shoup's modular arithmetic trick.

- [Link](https://www.nayuki.io/page/number-theoretic-transform-integer-dft): A simple NTT tutorial going through the basics.

- [Link](http://www.apfloat.org/ntt.html): Mikko Tommila's notes on the NTT.

- [Speeding up the number theoretic transform](https://eprint.iacr.org/2016/504.pdf) by Longa and Naehrig

- [The FFT - an algorithm the whole family can use](https://www.cs.dartmouth.edu/~rockmore/cse-fft.pdf): Just a fun paper title. Not useful for this project.

## Primes and Modular Arithmetic

- [Generalized Mersenne Primes](http://cacr.uwaterloo.ca/techreports/1999/corr99-39.pdf): Solinas gives a method for reducing modular reduction of "nice" moduli to adds, subtracts, and shifts. Finnicky to get right though. Understanding this paper requires you to know what a [LFSR](https://pdfs.semanticscholar.org/a47e/2c91605fd3f0753a736d26f3bf3d8e1ef548.pdf) is.

- [Montgomery Multiplication](https://cp-algorithms.com/algebra/montgomery_multiplication.html)


## Arbitrary-precision arithmetic software

Useful for rooting your nose around.

- [Link](https://en.wikipedia.org/wiki/List_of_arbitrary-precision_arithmetic_software): A general list.

- [Link](https://sourceforge.net/projects/bigz/): The `bigz` library, very easy to read, but not very sophisticated.

### Fabrice Bellard's libbf

This is an impressive and small library, in typical Bellard
style. It's quite difficult to read though.

- [Link](https://bellard.org/libbf/)

- [Technical notes](https://bellard.org/libbf/readme.txt)

- [GitHub Mirror](https://github.com/rurban/libbf)


## Assembly and SBCL

Paul Khuong has a wonderful series on writing low-level code (read: assembly) in SBCL.

- [SBCL: The Ultimate Assembly Code Breadboard](https://www.pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/)

- [How to Define New Intrinsics](https://www.pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/)

- [SSE Intrinsics](https://www.pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/)

Quick x864-64 assembly references:

- [Link](https://www.felixcloutier.com/x86/index.html)

- [godbolt](https://godbolt.org/) Useful for checking C code and getting assembler inspiration.


## Closed Source Mathematical Software :(

The authors of these programs have done very impressive things. So
impressive that they feel their computational secrets must not be
shared. Too bad; the world is better when math software is open.

- [y-cruncher](http://www.numberworld.org/y-cruncher/): Purportedly the best pi calculating program on the planet. Broke many a record.

- [PiFast](http://numbers.computation.free.fr/Constants/PiProgram/pifast.html): Was the fastest program for a long time. 

- [SuperPi](http://www.superpi.net/): Port of a program that broke a record in the 90s. Became popular with benchmarkers.

## Out-of-Core FFTs

- [Fast Fourier Transforms---For Fun and Profit](http://www.cis.rit.edu/class/simg716/FFT_Fun_Profit.pdf): The OG paper about it.

- [Determining an Out-of-Core FFT Decomposition Strategy](https://pdfs.semanticscholar.org/30e3/07cc26b038b654122426133d6d545d2cc7e7.pdf) by Thomas Cormen: Has info about Swarztrauber's method and out-of-core FFTs.

- [FFTs in External or Hierarchical Memory](https://www.davidhbailey.com/dhbpapers/fftq.pdf) by David Bailey.

- See also Arndt.

## Miscellaneous links

- [Numbers, Constants, and Computation](http://numbers.computation.free.fr/Constants/constants.html): A classic website (all the way back to 1999!) by Gourdon and Sebah. I remember reading this when I was just starting to get into math & programming.

- [Jason Papadopoulos's pages](https://web.archive.org/web/20160307010247/http://www.boo.net/~jasonp/): Pi programs, FFTs, etc. Another early inspiration for my study.

- [Modern Computer Arithmetic](https://members.loria.fr/PZimmermann/mca/mca-cup-0.5.9.pdf) by Brent and Zimmerman
