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
