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
module Eff.Exception (
  Exc(..),
  throwError,
  runError,
  catchError
) where

import Eff.Internal
import Eff.Functor (CoEff (effmap), transformEff)


--------------------------------------------------------------------------------
                           -- Exceptions --
--------------------------------------------------------------------------------
-- | Exceptions of the type e; no resumption
newtype Exc e v = Exc e

instance CoEff Exc where
  effmap f = transformEff $ \arr -> \case
    Exc e -> send (Exc $ f e) >>= arr

-- | Throws an error carrying information of type e
throwError :: (Member (Exc e) r) => e -> Eff r a
throwError e = send (Exc e)

-- | Handler for exception effects
-- If there are no exceptions thrown, returns Right If exceptions are
-- thrown and not handled, returns Left, interrupting the execution of
-- any other effect handlers.
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError =
   handleRelay (return . Right) (\ (Exc e) _k -> return (Left e))

-- | A catcher for Exceptions. Handlers are allowed to rethrow
-- exceptions.
catchError :: Member (Exc e) r =>
        Eff r a -> (e -> Eff r a) -> Eff r a
catchError m handle = interpose return (\(Exc e) _k -> handle e) m
