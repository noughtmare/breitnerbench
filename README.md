# Benchmark of Joachim Breitner's Haskell Love Talk

I watched Joachim Breitner's Haskell Love talk today. He starts with a high level specification of a program that checks if a binary tree is ordered. During the talk he develops many different implementations. I thought that it would be interesting to see how fast all the different implementations really are. It was surprising for me that the slowest implementation was only about 14 times slower than the fastest implementation. I expected a bigger difference.

Here is the link to the talk: https://vimeo.com/442720683#t=7067s

Breitner's talk starts around 1:57:47.

Currently, I only test the functions on balanced ordered trees with 100
elements from 0 to 100.

## Run these yourself

My machine may produce different results than yours.

Find out by cloning this repo and running it with cabal:

```
git clone https://github.com/noughtmare/breitnerbench.git
cd breitnerbench
cabal run
```

## On my machine

The fastest is `isOrdered5` with a close runner up being `isOrdered12`.

The slowest are `isOrdered1` and `isOrdered2`, which seem to be compiled to the
same optimized binary code.

Here are all the results with GHC 8.10.1 and `-O2`:

```
benchmarked isOrdered1/100
time                 11.62 μs   (11.21 μs .. 12.36 μs)
                     0.982 R²   (0.971 R² .. 0.991 R²)
mean                 11.25 μs   (11.08 μs .. 11.51 μs)
std dev              707.9 ns   (521.0 ns .. 945.8 ns)
variance introduced by outliers: 38% (moderately inflated)

benchmarked isOrdered2/100
time                 11.99 μs   (11.02 μs .. 12.78 μs)
                     0.980 R²   (0.974 R² .. 0.992 R²)
mean                 11.56 μs   (11.35 μs .. 11.84 μs)
std dev              854.7 ns   (665.9 ns .. 1.228 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarked isOrdered3/100
time                 7.343 μs   (6.870 μs .. 7.803 μs)
                     0.980 R²   (0.970 R² .. 0.991 R²)
mean                 7.264 μs   (7.145 μs .. 7.397 μs)
std dev              435.2 ns   (376.2 ns .. 570.9 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarked isOrdered4/100
time                 7.065 μs   (6.582 μs .. 7.746 μs)
                     0.963 R²   (0.940 R² .. 0.987 R²)
mean                 7.923 μs   (7.686 μs .. 8.211 μs)
std dev              904.6 ns   (695.7 ns .. 1.309 μs)
variance introduced by outliers: 69% (severely inflated)

benchmarked isOrdered5/100
time                 836.9 ns   (806.0 ns .. 880.2 ns)
                     0.980 R²   (0.964 R² .. 0.992 R²)
mean                 921.9 ns   (900.0 ns .. 947.5 ns)
std dev              84.75 ns   (70.42 ns .. 107.7 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarked isOrdered6/100
time                 7.305 μs   (6.808 μs .. 7.997 μs)
                     0.943 R²   (0.919 R² .. 0.965 R²)
mean                 9.828 μs   (9.436 μs .. 10.21 μs)
std dev              1.357 μs   (1.147 μs .. 1.679 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarked isOrdered7/100
time                 4.075 μs   (3.821 μs .. 4.351 μs)
                     0.987 R²   (0.978 R² .. 1.000 R²)
mean                 3.888 μs   (3.858 μs .. 3.977 μs)
std dev              147.3 ns   (83.19 ns .. 293.5 ns)
variance introduced by outliers: 20% (moderately inflated)

benchmarked isOrdered8/100
time                 3.933 μs   (3.919 μs .. 3.955 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.945 μs   (3.937 μs .. 3.958 μs)
std dev              31.57 ns   (21.44 ns .. 58.89 ns)

benchmarked isOrdered9/100
time                 2.865 μs   (2.849 μs .. 2.882 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.860 μs   (2.854 μs .. 2.872 μs)
std dev              27.32 ns   (18.64 ns .. 45.24 ns)

benchmarked isOrdered10/100
time                 1.870 μs   (1.864 μs .. 1.878 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.872 μs   (1.870 μs .. 1.875 μs)
std dev              9.259 ns   (7.656 ns .. 11.46 ns)

benchmarked isOrdered11/100
time                 2.408 μs   (2.400 μs .. 2.416 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.418 μs   (2.414 μs .. 2.422 μs)
std dev              12.13 ns   (9.753 ns .. 15.91 ns)

benchmarked isOrdered12/100
time                 915.5 ns   (912.5 ns .. 918.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 916.2 ns   (915.0 ns .. 919.3 ns)
std dev              6.027 ns   (3.736 ns .. 10.55 ns)

benchmarked isOrdered13/100
time                 1.347 μs   (1.344 μs .. 1.351 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.352 μs   (1.350 μs .. 1.355 μs)
std dev              8.979 ns   (6.977 ns .. 11.59 ns)
```
