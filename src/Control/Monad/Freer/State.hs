{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Control.Monad.Freer.State
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
module Control.Monad.Freer.State (
  State,
  get,
  put,
  modify,
  runState,

  transactionState
) where

import Control.Monad.Freer.Internal
import Control.Monad.Freer.Functor (InvEff (inveffmap), transformEff)
import Data.Proxy

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

-- | Handler for State effects
runState :: s -> Eff (State s ': r) w -> Eff r (w,s)
runState s (Val x) = return (x,s)
runState s (E u q) = case decomp u of
  Right Get      -> runState s (qApp q s)
  Right (Put s') -> runState s' (qApp q ())
  Left  u'       -> E u' (tsingleton (\x -> runState s (qApp q x)))


-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s r w. Member (State s) r =>
                    Proxy s -> Eff r w -> Eff r w
transactionState _ m = do s <- get; loop s m
 where
   loop :: s -> Eff r w -> Eff r w
   loop s (Val x) = put s >> return x
   loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
     Just Get      -> loop s (qApp q s)
     Just (Put s') -> loop s'(qApp q ())
     _             -> E u (tsingleton k) where k = qComp q (loop s)
