{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Tests.Coroutine (
  runTestCoroutine
) where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Coroutine
import Control.Monad.Freer.State

runTestCoroutine :: [Int] -> Int
runTestCoroutine list = snd $ run $ runState 0 effTestCoroutine
  where
    testCoroutine :: (Members '[Yield () Int, State Int] r) => Eff r ()
    testCoroutine = do
      -- yield for two elements and hope they're both odd
      b <- (&&)
        <$> yield () (even :: Int -> Bool)
        <*> yield () (even :: Int -> Bool)
      unless b (modify ((+1) :: Int -> Int) >> testCoroutine)

    effTestCoroutine = do
      status <- runC testCoroutine
      handleStatus list status
        where
          handleStatus _ (Done ()) = return ()
          handleStatus (i:is) (Continue () k) = k i >>= handleStatus is
          handleStatus [] _ = return ()
