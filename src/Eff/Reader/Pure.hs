{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Eff.Reader.Pure
  ( module Eff.Reader
  , module Eff.Reader.Pure
  ) where

import Eff
import Eff.Reader

-- | Handler for reader effects
runReader :: e -> Eff (Reader e ': r) w -> Eff r w
runReader e = handleRelay return $ \Reader k -> k e
