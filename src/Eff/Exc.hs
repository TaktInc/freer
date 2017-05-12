{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : Eff.Exception
Description : An Exception effect and handler.
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Exception effects. Communicates success/failure
via an Either type.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Eff.Exc
  ( module Eff.Exc
  ) where

import Control.Arrow (left)
import Eff
import Eff.Functor (CoEff (effmap), transformEff)


--------------------------------------------------------------------------------
                           -- Exceptions --
--------------------------------------------------------------------------------
-- | Exceptions of the type e; no resumption
newtype Exc e v = Exc e

instance CoEff Exc where
  effmap f = transformEff $ \arr -> \case
    Exc e -> send (Exc $ f e) >>= arr


------------------------------------------------------------------------------
-- | Throws an error carrying information of type e
throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send (Exc e)


------------------------------------------------------------------------------
-- | Tag the 'Nothing' value of a 'Maybe' by throwing an 'Exc'.
remark :: Member (Exc e) r => e -> Maybe v -> Eff r v
remark e = maybe (throwError e) pure


------------------------------------------------------------------------------
-- | Bind and tag the 'Nothing' value of a 'Maybe' by throwing an 'Exc'.
remarkM :: Member (Exc e) r => e -> Eff r (Maybe v) -> Eff r v
remarkM e mmv = mmv >>= remark e


------------------------------------------------------------------------------
-- | Flipped 'remark'. Meant to be used infix:
-- @mv `orThrowError` e@
orThrowError :: Member (Exc e) r => Maybe v -> e -> Eff r v
orThrowError = flip remark


------------------------------------------------------------------------------
-- | Flipped 'remarkM'. Meant to be used infix:
-- @mmv `orThrowErrorM` e@
orThrowErrorM :: Member (Exc e) r => Eff r (Maybe v) -> e -> Eff r v
orThrowErrorM = flip remarkM


------------------------------------------------------------------------------
-- | Tag the 'Left' value of an 'Either' by throwing it as an 'Exc'.
squelch :: Member (Exc e) r => Either e v -> Eff r v
squelch = squelching id


------------------------------------------------------------------------------
-- | Tag the 'Left' value of an 'Either' by transforming it into an 'Exc'.
squelching :: Member (Exc e) r => (e' -> e) -> Either e' v -> Eff r v
squelching = (either throwError pure .) . left
