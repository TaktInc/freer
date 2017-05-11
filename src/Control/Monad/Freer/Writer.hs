{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeOperators    #-}

{-|
Module      : Control.Monad.Freer.Writer
Description : Composable Writer effects -
Copyright   : Allele Dev 2016
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Writer effects, for writing changes to an attached environment.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.Writer (
  Writer(..),
  tell,
  runWriter
) where

#if __GLASGOW_HASKELL__ <= 708
import Data.Monoid
#endif

import Control.Monad.Freer.Internal
import Control.Monad.Freer.Functor (CoEff (effmap), transformEff)

-- | Writer effects - send outputs to an effect environment
data Writer o x where
  Writer :: o -> Writer o ()

instance CoEff Writer where
  effmap f = transformEff $ \arr -> \case
    Writer w -> send (Writer $ f w) >>= arr

-- | Send a change to the attached environment
tell :: Member (Writer o) r => o -> Eff r ()
tell o = send $ Writer o

-- | Simple handler for Writer effects
runWriter :: Monoid o => Eff (Writer o ': r) a -> Eff r (a,o)
runWriter = handleRelay (\x -> return (x,mempty))
                  (\ (Writer o) k -> k () >>= \ (x,l) -> return (x,o `mappend` l))
