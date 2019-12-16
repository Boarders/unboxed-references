{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
          C.bench "Unpacked/modifySTRef : counter"  $ C.whnf counterSTRef   1000000
        , C.bench "Unpacked/modifySTRef': counter"  $ C.whnf counterSTRef'  1000000
        , C.bench "Unpacked/modifySTRefU: counter"  $ C.whnf counterSTRefU  1000000
        , C.bench "lazy/modifySTRef : counter"      $ C.whnf counterSTRefL  1000000
        , C.bench "lazy/modifySTRef': counter"      $ C.whnf counterSTRefL' 1000000
        , C.bench "lazy/modifySTRefU: counter"      $ C.whnf counterSTRefUL 1000000
       ]
     ]

counterSTRef :: Int -> Point
counterSTRef n =
  runST $
    do
      ref <- newSTRef (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef ref increment
      readSTRef ref

counterSTRefL :: Int -> PointL
counterSTRefL n =
  runST $
    do
      ref <- newSTRef (PointL 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef ref incrementL
      readSTRef ref


counterSTRef' :: Int -> Point
counterSTRef' n =
  runST $
    do
      ref <- newSTRef (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef' ref increment
      readSTRef ref


counterSTRefL' :: Int -> PointL
counterSTRefL' n =
  runST $
    do
      ref <- newSTRef (PointL 0 0 0)
      for_ [1..n] $ \_ -> modifySTRef' ref incrementL
      readSTRef ref


counterSTRefU :: Int -> Point
counterSTRefU n =
  runST $
    do
      ref <- newSTRefU (Point 0 0 0)
      for_ [1..n] $ \_ -> modifySTRefU ref increment
      readSTRefU ref


counterSTRefUL :: Int -> PointL
counterSTRefUL n =
  runST $
    do
      ref <- newSTRefU (PointL 0 0 0)
      for_ [1..n] $ \_ -> modifySTRefU ref incrementL
      readSTRefU ref


data Point = Point
  { x :: {-# UNPACK #-} !Int
  , y :: {-# UNPACK #-} !Int
  , z :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data PointL = PointL
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving (Eq, Show)

incrementL :: PointL -> PointL
incrementL (PointL x y z) = PointL (x + 1) (y + 1) (z + 1)

increment :: Point -> Point
increment (Point x y z) = Point (x + 1) (y + 1) (z + 1)


derivingUnbox "Point"
   [t| Point -> (Int, Int, Int)    |]
   [| \ (Point x y z) -> (x, y, z) |]
   [| \ (x, y, z) -> Point x y z   |]


derivingUnbox "PointL"
   [t| PointL -> (Int, Int, Int)    |]
   [| \ (PointL x y z) -> (x, y, z) |]
   [| \ (x, y, z) -> PointL x y z   |]


