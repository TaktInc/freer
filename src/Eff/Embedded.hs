{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Eff.Embedded (
  Embedded(..),
  Raisable(..),
  embed,
  raiseEmbedded,
  liftEmbedded,
  runEmbedded,
  runEmbeddedAsync
) where

import Control.Concurrent.Async (async)
import Control.Monad

import Eff.Internal


data Embedded ms a where
  Embed :: Eff ms () -> Embedded ms ()

embed :: forall ms r. (Member (Embedded ms) r) => Eff ms () -> Eff r ()
embed = send . Embed

class Raisable (ms :: [* -> *]) r where
  raiseUnion :: Union ms a -> Eff r a

instance Raisable '[] r where
  raiseUnion _ = error "absurd: raiseUnion run on an empty union"

instance (Member e r, Raisable m r) =>  Raisable (e ': m) r where
  raiseUnion u =
    case decomp u of
      Right x -> send x
      Left u' -> raiseUnion u'

raiseEmbedded :: Raisable m r => Eff m a -> Eff r a
raiseEmbedded = loop
  where
    loop (Val x)  = pure x
    loop (E u' q) = raiseUnion u' >>= qComp q loop

liftEmbedded :: (Raisable m r) => Eff (Embedded m ': r) a -> Eff r a
liftEmbedded = runEmbedded void

runEmbedded :: (Raisable m r)
            => (forall v. Eff r v -> Eff r' ())
            -> Eff (Embedded m ': r') a
            -> Eff r' a
runEmbedded f = handleRelay pure $ \(Embed e) -> (f (raiseEmbedded e) >>=)

runEmbeddedAsync :: (Raisable m d, Member IO r)
                 => (forall v. Eff d v -> IO v)
                 -> Eff (Embedded m ': r) a
                 -> Eff r a
runEmbeddedAsync f = runEmbedded (send @IO . void . async . f)

