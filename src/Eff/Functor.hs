{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Eff.Functor where

import Control.Applicative (pure)
import Eff.Internal
import GHC.Exts
import Data.Proxy
import Unsafe.Coerce


-- | A class witnessing that 'eff' forms a covariant functor in its second type
-- parameter.
class CoEff eff where
  effmap :: Sub (eff a) (eff b) r r' => Proxy eff -> (a -> b) -> Eff r x -> Eff r' x


-- | A class witnessing that 'eff' forms a contravariant functor in its second
-- type parameter.
class ContraEff eff where
  contraeffmap :: Sub (eff a) (eff b) r r' => Proxy eff -> (b -> a) -> Eff r x -> Eff r' x


-- | A class witnessing that 'eff' forms a invariant functor in its second
-- type parameter.
class InvEff eff where
  inveffmap :: (a -> b) -> (b -> a) -> Eff (eff a ': r) x -> Eff (eff b ': r) x


-- | Helper function for defining 'effmap', 'contraeffmap', and 'inveffmap' by
-- wrangling 'replaceRelay's type into something that can be type-inferred and
-- lambda-cased.
transformEff :: Sub (eff a) (eff b) r r'
             => Proxy b
             -> (forall v. (v -> Eff r' x) -> eff a v -> Eff r' x)
             -> Eff r x
             -> Eff r' x
transformEff _ f = loop
  where
   loop (Val x)  = pure x
   loop (E u q)  = case prj u of
     Just x -> f k x
     _      -> E (unsafeCoerce u) (tsingleton k)
    where k = qComp q loop


type family CopyAllBut (r :: [* -> *]) (e :: * -> *) (r' :: [* -> *])  :: Constraint where
  CopyAllBut '[] x r' = ()
  CopyAllBut (re ': res) re r' = CopyAllBut res re r'
  CopyAllBut (re ': res) e r' = (Member re r', CopyAllBut res e r')

type Sub (e :: * -> *) (e' :: * -> *) r r' = (CopyAllBut r e r', Member e r, Member e' r')

-- -- | Helper function for defining 'effmap', 'contraeffmap', and 'inveffmap' by
-- -- wrangling 'replaceRelay's type into something that can be type-inferred and
-- -- lambda-cased.
-- transformEff :: (forall v. (v -> Eff (eff b ': r) x)
--                         -> eff a v
--                         -> Eff (eff b ': r) x
--                 )
--              -> Eff (eff a ': r) x
--              -> Eff (eff b ': r) x
-- transformEff f = replaceRelay pure $ flip f
