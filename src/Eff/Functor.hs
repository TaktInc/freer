{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Eff.Functor where

import Control.Applicative (pure)
import Eff.Internal (Eff, replaceRelay)


-- | A class witnessing that 'eff' forms a covariant functor in its second type
-- parameter.
class CoEff eff where
  effmap :: (a -> b) -> Eff (eff a ': r) x -> Eff (eff b ': r) x


-- | A class witnessing that 'eff' forms a contravariant functor in its second
-- type parameter.
class ContraEff eff where
  contraeffmap :: (b -> a) -> Eff (eff a ': r) x -> Eff (eff b ': r) x


-- | A class witnessing that 'eff' forms a invariant functor in its second
-- type parameter.
class InvEff eff where
  inveffmap :: (a -> b) -> (b -> a) -> Eff (eff a ': r) x -> Eff (eff b ': r) x


-- | Helper function for defining 'effmap', 'contraeffmap', and 'inveffmap' by
-- wrangling 'replaceRelay's type into something that can be type-inferred and
-- lambda-cased.
transformEff :: (forall v. (v -> Eff (eff b ': r) x)
                        -> eff a v
                        -> Eff (eff b ': r) x
                )
             -> Eff (eff a ': r) x
             -> Eff (eff b ': r) x
transformEff f = replaceRelay pure $ flip f

