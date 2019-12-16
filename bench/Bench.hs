{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where


import           Control.Monad.ST
import qualified Criterion.Main               as C (bench, bgroup, defaultMain,
                                                    whnf)
import           Data.Foldable
import           Data.STRef
import           Data.STRef.Unboxed
import           Data.Vector.Unboxed.Deriving


main :: IO ()
main = C.defaultMain
     [ C.bgroup "Reference Cell"
       [
          C.bench "modifySTRef : counter"  $ C.whnf counterSTRef  1000000
        , C.bench "modifySTRef': counter"  $ C.whnf counterSTRef' 1000000
        , C.bench "modifySTRefU: counter"  $ C.whnf counterSTRefU 1000000
       ]
     ]

counterSTRef :: Int -> Point
counterSTRef n =
  runST $
    do
      ref <- newSTRef (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef ref increment
      readSTRef ref


counterSTRef' :: Int -> Point
counterSTRef' n =
  runST $
    do
      ref <- newSTRef (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef' ref increment
      readSTRef ref

counterSTRefU :: Int -> Point
counterSTRefU n =
  runST $
    do
      ref <- newSTRefU (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRefU ref increment
      readSTRefU ref


data Point = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  , z :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

increment :: Point -> Point
increment (Point x y z) = Point (x + 1) (y + 1) (z + 1)

derivingUnbox "Point"
   [t| Point -> (Int, Int, Int)    |]
   [| \ (Point x y z) -> (x, y, z) |]
   [| \ (x, y, z) -> Point x y z   |]


