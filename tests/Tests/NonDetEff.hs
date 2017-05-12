{-# LANGUAGE FlexibleContexts #-}
module Tests.NonDetEff where

import Control.Applicative
import Control.Monad
import Eff

ifte :: Member NonDetEff r
     => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= maybe el (\(a,m) -> th a <|> (m >>= th))

generatePrimes :: Member NonDetEff r => [Int] -> Eff r Int
generatePrimes xs = do
  n <- gen
  ifte (do d <- gen
           guard $ d < n && n `mod` d == 0)
       (const mzero)
       (return n)
  where gen = msum (fmap return xs)

testIfte :: [Int] -> [Int]
testIfte = run . makeChoiceA . generatePrimes
