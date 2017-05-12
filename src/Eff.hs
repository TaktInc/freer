{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Eff
Description : Freer - an extensible effects library
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Eff (
  Member,
  Members,
  Eff,
  run,
  runM,
  runNat,
  handleRelay,
  handleRelayS,
  replaceRelay,
  replaceRelayS,
  raise,
  send,
  Arr,

  NonDetEff(..),
  makeChoiceA,
  msplit
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (pure)
#endif

import Eff.Internal

runNat
  :: forall m r e w.
     (Member m r)
  => (forall a. e a -> m a) -> Eff (e ': r) w -> Eff r w
runNat f = handleRelay pure (\v -> (send (f v) >>=))
