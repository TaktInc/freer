{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.State (
  testPutGet,
  testPutGetPutGetPlus,
  testGetStart,
  testInveffmap
) where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Functor (inveffmap)
import Data.Tuple (swap)

testPutGet :: Int -> Int -> (Int,Int)
testPutGet n start = run (runState go start)
  where go = put n >> get

testPutGetPutGetPlus :: Int -> Int -> Int -> (Int,Int)
testPutGetPutGetPlus p1 p2 start = run (runState go start)
  where go = do
          put p1
          x <- get
          put p2
          y <- get
          return (x+y)

testGetStart :: Int -> (Int,Int)
testGetStart = run . runState get

testInveffmap :: (Int, String) -> String
testInveffmap n = fst . run $ flip runState (0 :: Int, "hello") $ inveffmap swap swap go
  where
    go = do
      put $ swap n
      (s, i :: Int) <- get
      pure $ show i ++ s
