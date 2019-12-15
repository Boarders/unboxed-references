{-|
Unboxed mutable references backed by unboxed vectors.
-}

module Data.STRef.Unboxed
  ( -- * Unboxed ST References
    STRefU(..)
  , newSTRefU
  , readSTRefU
  , writeSTRefU
  , modifySTRefU
  ) where

import           Control.Monad.ST
import           Data.Coerce
import           Data.Vector.Unboxed.Mutable (MVector, Unbox)
import qualified Data.Vector.Unboxed.Mutable as Unboxed


-- |
-- An unboxed mutable variable that can hold any type which is
-- an instance of 'Unbox'. This is backed by a length 1 unboxed
-- mutable vector.
newtype STRefU s a = STRefU {getSTRefU :: MVector s a}


-- |
-- Create a new `STRefU` containing the given value.
newSTRefU :: (Unbox a) => a -> ST s (STRefU s a)
newSTRefU a =
  do
    mv <- Unboxed.unsafeNew 1
    Unboxed.write mv 0 a
    pure (coerce mv)
-- |
-- Read the value in a reference cell.
readSTRefU :: (Unbox a) => STRefU s a -> ST s a
readSTRefU (STRefU mv) = Unboxed.read mv 0


-- |
-- Write a value to a reference cell.
writeSTRefU :: (Unbox a) => STRefU s a -> a -> ST s ()
writeSTRefU (STRefU mv) = Unboxed.write mv 0


-- |
-- Modify the value in a reference cell. This is strict in its argument.
modifySTRefU :: (Unbox a) => STRefU s a -> (a -> a) -> ST s ()
modifySTRefU (STRefU mv) f = Unboxed.modify mv f 0
