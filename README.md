# Benchmark of Joachim Breitner's Haskell Love Talk

I watched Joachim Breitner's Haskell Love talk today. He starts with a high level specification of a program that checks if a binary tree is ordered. During the talk he develops many different implementations. I thought that it would be interesting to see how fast all the different implementations really are.

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

The fastest is `isOrdered12`, but `isOrdered5` can be specialized to `Int`, the tuple can be changed to two arguments and the max and min bounds can be used instead of using a maybe type.

The slowest is `isOrdered6`.

Here are all the results with GHC 8.10.1 and `-O2`:

```
benchmarked isOrdered1/10000
time                 3.445 ms   (3.380 ms .. 3.491 ms)
                     0.994 R²   (0.986 R² .. 0.998 R²)
mean                 3.578 ms   (3.532 ms .. 3.657 ms)
std dev              198.4 μs   (129.8 μs .. 304.4 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarked isOrdered2/10000
time                 3.723 ms   (3.600 ms .. 3.897 ms)
                     0.990 R²   (0.977 R² .. 0.999 R²)
mean                 3.569 ms   (3.537 ms .. 3.649 ms)
std dev              159.3 μs   (88.03 μs .. 272.5 μs)
variance introduced by outliers: 24% (moderately inflated)

benchmarked isOrdered3/10000
time                 1.120 ms   (1.116 ms .. 1.124 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.122 ms   (1.120 ms .. 1.126 ms)
std dev              10.33 μs   (6.950 μs .. 16.89 μs)

benchmarked isOrdered4/10000
time                 1.592 ms   (1.589 ms .. 1.597 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.597 ms   (1.594 ms .. 1.601 ms)
std dev              10.84 μs   (7.971 μs .. 15.27 μs)

benchmarked isOrdered5/10000
time                 198.1 μs   (197.6 μs .. 198.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 198.8 μs   (198.6 μs .. 199.1 μs)
std dev              1.004 μs   (840.1 ns .. 1.228 μs)

benchmarked isOrdered5 (specialized)/10000
time                 97.43 μs   (97.17 μs .. 97.69 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 97.49 μs   (97.34 μs .. 97.66 μs)
std dev              525.1 ns   (439.6 ns .. 665.8 ns)

benchmarked isOrdered5 (specialized & curried)/10000
time                 73.18 μs   (73.00 μs .. 73.40 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 73.35 μs   (73.24 μs .. 73.48 μs)
std dev              424.3 ns   (342.0 ns .. 562.4 ns)

benchmarked isOrdered5 (specialized & curried & bounded)/10000
time                 62.57 μs   (61.99 μs .. 63.09 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 62.85 μs   (62.62 μs .. 63.06 μs)
std dev              757.5 ns   (592.2 ns .. 911.6 ns)

benchmarked isOrdered6/10000
time                 4.464 ms   (4.433 ms .. 4.499 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 4.486 ms   (4.474 ms .. 4.500 ms)
std dev              41.40 μs   (33.11 μs .. 51.08 μs)

benchmarked isOrdered7/10000
time                 1.790 ms   (1.783 ms .. 1.795 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.777 ms   (1.772 ms .. 1.782 ms)
std dev              16.42 μs   (12.42 μs .. 22.69 μs)

benchmarked isOrdered8/10000
time                 2.770 ms   (2.758 ms .. 2.786 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.765 ms   (2.759 ms .. 2.773 ms)
std dev              23.42 μs   (17.08 μs .. 31.73 μs)

benchmarked isOrdered9/10000
time                 1.013 ms   (1.009 ms .. 1.016 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.012 ms   (1.010 ms .. 1.015 ms)
std dev              8.793 μs   (7.130 μs .. 10.91 μs)

benchmarked isOrdered10/10000
time                 270.2 μs   (268.6 μs .. 271.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 269.5 μs   (268.8 μs .. 270.4 μs)
std dev              2.587 μs   (2.123 μs .. 3.173 μs)

benchmarked isOrdered11/10000
time                 660.9 μs   (657.8 μs .. 664.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 661.7 μs   (660.2 μs .. 663.6 μs)
std dev              5.611 μs   (4.348 μs .. 8.156 μs)

benchmarked isOrdered12/10000
time                 96.61 μs   (96.20 μs .. 97.10 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 96.97 μs   (96.70 μs .. 97.41 μs)
std dev              1.130 μs   (779.0 ns .. 1.901 μs)

benchmarked isOrdered13/10000
time                 141.7 μs   (141.2 μs .. 142.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 142.0 μs   (141.7 μs .. 142.4 μs)
std dev              1.298 μs   (1.033 μs .. 1.740 μs)
```
