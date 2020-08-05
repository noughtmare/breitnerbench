# Benchmark of Joachim Breitner's Haskell Love Talk

I watched Joachim Breitner's Haskell Love talk today. He starts with a high level specification of a program that checks if a binary tree is ordered. During the talk he develops many different implementations. I thought that it would be interesting to see how fast all the different implementations really are. It was surprising for me that the slowest implementation was only about 14 times slower than the fastest implementation. I expected a bigger difference.

Here is the link to the talk: https://vimeo.com/442720683#t=7067s

Breitner's talk starts around 1:57:47.

Currently, I only test the functions on balanced ordered trees with 10000
elements from 0 to 10000.

## Run these yourself

My machine may produce different results than yours.

Find out by cloning this repo and running it with cabal:

```
git clone https://github.com/noughtmare/breitnerbench.git
cd breitnerbench
cabal run -O2
```

## On my machine

The fastest is `isOrdered5` with a close runner up being `isOrdered12`.

The slowest are `isOrdered1` and `isOrdered2`, which seem to be compiled to the
same optimized binary code.

Here are all the results with GHC 8.10.1 and `-O2`:

```
benchmarked isOrdered1/10000
time                 3.555 ms   (3.536 ms .. 3.572 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.557 ms   (3.550 ms .. 3.568 ms)
std dev              26.91 μs   (20.89 μs .. 36.60 μs)

benchmarked isOrdered2/10000
time                 3.777 ms   (3.588 ms .. 3.981 ms)
                     0.989 R²   (0.982 R² .. 0.999 R²)
mean                 3.590 ms   (3.556 ms .. 3.661 ms)
std dev              148.1 μs   (62.03 μs .. 239.2 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarked isOrdered3/10000
time                 1.106 ms   (1.097 ms .. 1.119 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.179 ms   (1.163 ms .. 1.198 ms)
std dev              62.15 μs   (53.37 μs .. 71.91 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarked isOrdered4/10000
time                 1.660 ms   (1.607 ms .. 1.726 ms)
                     0.993 R²   (0.989 R² .. 0.998 R²)
mean                 1.593 ms   (1.581 ms .. 1.616 ms)
std dev              57.92 μs   (40.94 μs .. 84.45 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarked isOrdered5/10000
time                 191.2 μs   (187.8 μs .. 195.0 μs)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 196.6 μs   (194.6 μs .. 200.6 μs)
std dev              9.247 μs   (6.358 μs .. 14.37 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarked isOrdered5 (specialized)/10000
time                 95.24 μs   (93.55 μs .. 97.27 μs)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 101.0 μs   (99.27 μs .. 103.2 μs)
std dev              7.008 μs   (5.783 μs .. 8.958 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarked isOrdered6/10000
time                 4.353 ms   (4.283 ms .. 4.438 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 4.725 ms   (4.631 ms .. 4.877 ms)
std dev              366.5 μs   (235.2 μs .. 548.2 μs)
variance introduced by outliers: 47% (moderately inflated)

benchmarked isOrdered7/10000
time                 1.754 ms   (1.723 ms .. 1.812 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 1.750 ms   (1.732 ms .. 1.776 ms)
std dev              67.56 μs   (49.90 μs .. 86.69 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarked isOrdered8/10000
time                 2.778 ms   (2.705 ms .. 2.828 ms)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 2.790 ms   (2.762 ms .. 2.833 ms)
std dev              117.4 μs   (90.78 μs .. 163.3 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarked isOrdered9/10000
time                 1.134 ms   (1.065 ms .. 1.204 ms)
                     0.986 R²   (0.979 R² .. 0.996 R²)
mean                 1.075 ms   (1.064 ms .. 1.096 ms)
std dev              51.60 μs   (37.62 μs .. 76.10 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarked isOrdered10/10000
time                 309.8 μs   (292.4 μs .. 324.2 μs)
                     0.986 R²   (0.979 R² .. 0.992 R²)
mean                 303.0 μs   (296.5 μs .. 321.4 μs)
std dev              32.82 μs   (16.88 μs .. 64.25 μs)
variance introduced by outliers: 67% (severely inflated)

benchmarked isOrdered11/10000
time                 709.3 μs   (666.2 μs .. 753.0 μs)
                     0.964 R²   (0.938 R² .. 0.984 R²)
mean                 761.4 μs   (739.2 μs .. 783.3 μs)
std dev              72.69 μs   (64.73 μs .. 83.83 μs)
variance introduced by outliers: 59% (severely inflated)

benchmarked isOrdered12/10000
time                 98.31 μs   (96.50 μs .. 100.2 μs)
                     0.994 R²   (0.989 R² .. 0.998 R²)
mean                 108.8 μs   (106.3 μs .. 111.6 μs)
std dev              8.823 μs   (7.718 μs .. 10.03 μs)
variance introduced by outliers: 54% (severely inflated)

benchmarked isOrdered13/10000
time                 138.7 μs   (137.8 μs .. 140.2 μs)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 138.4 μs   (137.8 μs .. 139.7 μs)
std dev              2.817 μs   (1.225 μs .. 4.685 μs)
```
