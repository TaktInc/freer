{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Eff.Exc.Pure
  ( module Eff.Exc
  , module Eff.Exc.Pure
  ) where

import Eff.Internal
import Eff.Exc

-- | Handler for exception effects
-- If there are no exceptions thrown, returns Right If exceptions are
-- thrown and not handled, returns Left, interrupting the execution of
-- any other effect handlers.
runError :: Eff (Exc e ': r) a -> Eff r (Either e a)
runError =
   handleRelay (return . Right) (\ (Exc e) _k -> return (Left e))

-- | A catcher for Exceptions. Handlers are allowed to rethrow
-- exceptions.
catchError :: Member (Exc e) r
           => Eff r a
           -> (e -> Eff r a)
           -> Eff r a
catchError m handle = interpose return (\(Exc e) _k -> handle e) m

onFail :: (e -> Eff r a) -> Eff (Exc e ': r) a -> Eff r a
onFail handler prg = do
  res <- runError prg
  either handler pure res
