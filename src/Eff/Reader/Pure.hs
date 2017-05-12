{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Eff.Reader.Pure
  ( module Eff.Reader
  , module Eff.Reader.Pure
  ) where

import Eff
import Eff.Reader



------------------------------------------------------------------------------
-- | Handler for reader effects
runReader :: e -> Eff (Reader e ': r) w -> Eff r w
runReader e = handleRelay return $ \Reader k -> k e


------------------------------------------------------------------------------
-- | Interpret a 'Reader' with a monadic action.
runReaderM :: forall s r a m
            . (Member m r)
           => m s
           -> Eff (Provider s ': r) a
           -> Eff r a
runReaderM mval = runNat nat
  where
    nat :: forall x. Provider s x -> m x
    nat Fetch = mval

