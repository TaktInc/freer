{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Eff.Reader
Description : Reader effects, for encapsulating an environment
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Reader effects. Handy for encapsulating an
environment with immutable state for interpreters.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Eff.Reader
  ( Reader(..)
  , ask
  , asks
  , local
  ) where

import Eff.Internal
import Eff.Functor (ContraEff (contraeffmap), transformEff)


-- |
data Reader e v where
  Reader :: Reader e e

instance ContraEff Reader where
  contraeffmap f = transformEff $ \arr -> \case
    Reader -> send Reader >>= arr . f

-- | Request a value for the environment
ask :: (Member (Reader e) r) => Eff r e
ask = send Reader

-- | Request a value from the environment and applys as function
asks :: (Member (Reader e) r) => (e -> b) -> Eff r b
asks f = ask >>= return . f

-- |
-- Locally rebind the value in the dynamic environment
-- This function is like a relay; it is both an admin for Reader requests,
-- and a requestor of them
local :: forall e a r. Member (Reader e) r =>
         (e -> e) -> Eff r a -> Eff r a
local f m = do
  e0 <- ask
  let e = f e0
  let h :: Reader e v -> Arr r v a -> Eff r a
      h Reader g = g e
  interpose return h m

