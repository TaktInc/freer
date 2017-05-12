{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

-- |
-- Module:       Eff.Coroutine
-- Description:  Composable coroutine effects layer.
-- Copyright:    (c) 2016 Allele Dev
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- An effect to compose functions with the ability to yield.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Eff.Coroutine
    ( Yield(..)
    , yield
    )
  where

import Eff.Internal
import Eff.Functor (ContraEff (contraeffmap), transformEff)


-- | A type representing a yielding of control.
--
-- Type variables have following meaning:
--
-- [@a@]
--   The current type.
--
-- [@b@]
--   The input to the continuation function.
--
-- [@c@]
--   The output of the continuation.
data Yield a b c = Yield a (b -> c)
  deriving (Functor)

instance ContraEff (Yield a) where
  contraeffmap f = transformEff $ \arr -> \case
    Yield a b -> send (Yield a $ b . f) >>= arr

-- | Lifts a value and a function into the Coroutine effect.
yield :: Member (Yield a b) effs => a -> (b -> c) -> Eff effs c
yield x f = send (Yield x f)
