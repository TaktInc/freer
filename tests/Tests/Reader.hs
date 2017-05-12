{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Tests.Reader (
  testReader,
  testMultiReader,
  testLocal,
  testEffmapReader
) where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Eff
import Eff.Functor (contraeffmap)
import Eff.Reader.Pure


import Tests.Common

--------------------------------------------------------------------------------
                            -- Examples --
--------------------------------------------------------------------------------
testReader :: Int -> Int -> Int
testReader n x = run . runReader n $ ask `add` pure x

{-
t1rr' = run t1
    No instance for (Member (Reader Int) Void)
      arising from a use of `t1'
-}

testMultiReader :: Float -> Int -> Float
testMultiReader f n = run . runReader f . runReader n $ t2
  where t2 = do
          v1 <- ask
          v2 <- ask
          return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- The opposite order of layers
{- If we mess up, we get an error
t2rrr1' = run $ runReader (runReader t2 (20::Float)) (10::Float)
    No instance for (Member (Reader Int) [])
      arising from a use of `t2'
-}

testLocal :: Int -> Int -> Int
testLocal env inc = run $ runReader env t3
  where t3 = t1 `add` local (+ inc) t1
        t1 = ask `add` return (1 :: Int)


testEffmapReader :: Int -> Int -> String
testEffmapReader n x = run . runReader n . contraeffmap show $ (++) <$> ask <*> pure (show x)

