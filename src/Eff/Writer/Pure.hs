{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Eff.Writer.Pure
  ( module Eff.Writer
  , module Eff.Writer.Pure
  ) where

import Eff
import Eff.Writer

-- | Simple handler for Writer effects
runWriter :: Monoid o => Eff (Writer o ': r) a -> Eff r (a, o)
runWriter = handleRelay (\x -> return (x, mempty))
                  (\ (Writer o) k -> k () >>= \ (x,l) -> return (x,o `mappend` l))

