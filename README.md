## `unboxed-references`

A library providing unboxed references backed by Unboxed mutable vectors.

As an example of how to use it:

```haskell
module Main where

import           Data.STRef.Unboxed
import           Control.Monad.ST
import           Data.Foldable
import qualified System.IO.Unsafe as Unsafe

main :: IO ()
main =
  do
    print myCounter
    print mySum
    print strictModify

-- |
-- Runs a for loop each time modifying the reference cell to add
-- one.
myCounter :: Int
myCounter =
  runST $
    do
      ref <- newSTRefU 0
      for_ ([1..100] :: [Int]) $ \_ -> modifySTRefU ref (+ 1)
      readSTRefU ref

-- |
-- Runs a for loop to add all the numbers between 1 and 10.
mySum :: Int
mySum =
  runST $
    do
      ref <- newSTRefU 0
      for_ [1..10] $ \i -> modifySTRefU ref (+ i)
      readSTRefU ref

-- |
-- This shows that modifySTRefU is strict in its argument.
strictModify :: Int
strictModify =
  runST $
    do
      ref <- newSTRefU 0
      modifySTRefU ref (\_ -> Unsafe.unsafePerformIO $ print "Got here!" >> pure 10)
      writeSTRefU ref 42
      readSTRefU ref

```

```haskell
> main
100
55
"Got here!"
42
```

Benchmarks:
```
benchmarking Reference Cell/Unpacked/modifySTRef : counter
time                 117.9 ms   (91.63 ms .. 140.5 ms)
                     0.959 R²   (0.933 R² .. 0.999 R²)
mean                 101.9 ms   (96.35 ms .. 110.2 ms)
std dev              10.42 ms   (6.236 ms .. 15.04 ms)
variance introduced by outliers: 31% (moderately inflated)

benchmarking Reference Cell/Unpacked/modifySTRef': counter
time                 3.368 ms   (3.242 ms .. 3.551 ms)
                     0.984 R²   (0.975 R² .. 0.995 R²)
mean                 3.394 ms   (3.340 ms .. 3.491 ms)
std dev              218.7 μs   (167.9 μs .. 290.9 μs)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Reference Cell/Unpacked/modifySTRefU: counter
time                 1.301 ms   (1.299 ms .. 1.305 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.304 ms   (1.301 ms .. 1.309 ms)
std dev              11.99 μs   (7.646 μs .. 17.01 μs)

benchmarking Reference Cell/lazy/modifySTRef : counter
time                 139.4 ms   (109.3 ms .. 180.5 ms)
                     0.955 R²   (0.839 R² .. 0.998 R²)
mean                 165.5 ms   (152.9 ms .. 176.6 ms)
std dev              16.57 ms   (11.38 ms .. 23.52 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Reference Cell/lazy/modifySTRef': counter
time                 80.73 ms   (77.22 ms .. 85.87 ms)
                     0.991 R²   (0.976 R² .. 0.997 R²)
mean                 88.28 ms   (83.67 ms .. 99.89 ms)
std dev              11.96 ms   (3.812 ms .. 20.14 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Reference Cell/lazy/modifySTRefU: counter
time                 1.302 ms   (1.294 ms .. 1.314 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.310 ms   (1.304 ms .. 1.327 ms)
std dev              32.13 μs   (10.27 μs .. 70.35 μs)
variance introduced by outliers: 13% (moderately inflated)
```
