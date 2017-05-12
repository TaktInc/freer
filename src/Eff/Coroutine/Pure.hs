{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Eff.Coroutine.Pure
  ( module Eff.Coroutine
  , module Eff.Coroutine.Pure
  ) where

import Eff.Internal
import Eff.Coroutine

-- | Represents status of a coroutine.
data Status effs a b x
    = Done x
    -- ^ Coroutine is done with a result value.
    | Continue a (b -> Eff effs (Status effs a b x))
    -- ^ Reporting a value of the type @a@, and resuming with the value of type
    -- @b@, possibly ending with a value of type @x@.

-- | Launch a coroutine and report its status.
runC :: Eff (Yield a b ': effs) w -> Eff effs (Status effs a b w)
runC = handleRelay (return . Done) replyC

-- | Launch a coroutine and report its status, without handling (removing)
-- `Yield` from the typelist. This is useful for reducing nested coroutines.
runC' :: Member (Yield a b) r => Eff r w -> Eff r (Status r a b w)
runC' = interpose (return . Done) replyC

-- | Reply to a coroutine effect by returning the Continue constructor.
replyC
  :: Yield a b c
  -> Arr r c (Status r a b w)
  -> Eff r (Status r a b w)
replyC (Yield a k) arr = return $ Continue a (arr . k)

