{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module      : Eff.Writer
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
module Eff.Writer
  ( Writer(..)
  , tell
) where

#if __GLASGOW_HASKELL__ <= 708
import Data.Monoid
#endif

import Eff.Internal
import Eff.Functor
import Data.Proxy

-- | Writer effects - send outputs to an effect environment
data Writer o x where
  Writer :: o -> Writer o ()

instance CoEff Writer where
  effmap :: forall a b r r' x. Sub (Writer a) (Writer b) r r'
         => Proxy Writer
         -> (a -> b)
         -> Eff r  x
         -> Eff r' x
  effmap _ f = transformEff (Proxy @b) $ \(arr :: v -> Eff r' x) -> \case
    (Writer w :: Writer a v) -> send (Writer $ f w) >>= arr

-- | Send a change to the attached environment
tell :: Member (Writer o) r => o -> Eff r ()
tell o = send $ Writer o

test1 :: Member (Writer Int) r => Eff r Int
test1 = tell (1 :: Int) >> return 0

test2 :: Member (Writer String) r => Eff r Int
test2 = effmap (Proxy @Writer) (show @Int) test1
