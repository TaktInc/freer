{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Eff.State
Description : State effects, for state-carrying computations.
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for State effects.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Eff.State
  ( State (..)
  , get
  , put
  , modify
  ) where

import Eff
import Eff.Functor (InvEff (inveffmap), transformEff)

--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

-- | Strict State effects: one can either Get values or Put them
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

instance InvEff State where
  inveffmap f g = transformEff $ \arr -> \case
    Get   -> send Get         >>= arr . g
    Put s -> send (Put $ f s) >>= arr


-- | Retrieve state
get :: Member (State s) r => Eff r s
get = send Get

-- | Store state
put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)

-- | Modify state
modify :: Member (State s) r => (s -> s) -> Eff r ()
modify f = fmap f get >>= put

