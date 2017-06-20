{-# LANGUAGE LambdaCase                        #-}
{-# LANGUAGE OverloadedStrings                 #-}
{-# LANGUAGE TemplateHaskell                   #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

-- | Automatic generation of freer monadic actions.
module Eff.TH
  ( makeFreer
  , makeFreer_
  ) where

import Control.Monad (forM, unless)
import Eff (send, Member, Eff)
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Prelude



------------------------------------------------------------------------------
-- | @$('makeFreer' ''T)@ provides freer monadic actions for the constructors of
-- the given GADT @T@.
makeFreer :: Name -> Q [Dec]
makeFreer = genFreer True


------------------------------------------------------------------------------
-- | Like 'makeFreer', but does not provide type signatures.
-- This can be used to attach Haddock comments to individual arguments
-- for each generated function.
--
-- @
-- data Lang x where
--   Output :: String -> Lang ()
--
-- makeFreer_ 'Lang
--
-- -- | Output a string.
-- output :: Member Lang effs
--        => String    -- ^ String to output.
--        -> Eff effs ()  -- ^ No result.
-- @
--
-- 'makeFreer_' must be called *before* the explicit type signatures.
makeFreer_ :: Name -> Q [Dec]
makeFreer_ = genFreer False


------------------------------------------------------------------------------
-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Eff' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer makeSigs tcName = do
  -- The signatures for the generated definitions require FlexibleContexts.
  isExtEnabled FlexibleContexts >>=
    flip unless (fail "makeFreer requires FlexibleContexts to be enabled")

  reify tcName >>= \case
    TyConI (DataD _ _ _ _ cons _) -> do
      sigs <- filter (const makeSigs) <$> mapM (genSig tcName) cons
      decs <- mapM genDecl cons
      return $ sigs ++ decs

    _ ->
      fail "makeFreer expects a type constructor"


------------------------------------------------------------------------------
-- | Given the name of a GADT constructor, return the name of the corresponding
-- lifted function.
getDeclName :: Name -> Name
getDeclName = mkName . overFirst toLower . nameBase
  where
    overFirst f (a:as) = f a : as
    overFirst _ as     = as


------------------------------------------------------------------------------
-- | Builds a function definition of the form @x a b c = send $ X a b c@.
genDecl :: Con -> Q Dec
genDecl (ForallC _ _ con) = genDecl con
genDecl (NormalC cName tArgs') = genDecl $ GadtC [cName] tArgs' undefined
genDecl (GadtC [cName] tArgs _) = do
  let fnName = getDeclName cName
  let arity = length tArgs - 1
  dTypeVars <- forM [0..arity] $ const $ newName "a"
  return $ FunD fnName
           . pure
           $ Clause (VarP <$> dTypeVars)
                    (NormalB . AppE (VarE 'send) $ foldl (\b -> AppE b . VarE) (ConE cName) dTypeVars)
                    []
genDecl _ = fail "genDecl expects a GADT constructor"


------------------------------------------------------------------------------
-- | Generates a type signature of the form
-- @x :: Member (Effect e) effs => a -> b -> c -> Eff effs r@.
genSig :: Name -> Con -> Q Dec
genSig eff (ForallC _ _ con) = genSig eff con
genSig eff (NormalC cName tArgs') = do
  var <- newName "a"
  genSig eff . GadtC [cName] tArgs' $ AppT (ConT eff) (VarT var)

genSig _ (GadtC [cName] tArgs' ctrType) = do
  effs <- newName "effs"
  let fnName           = getDeclName cName
      tArgs            = fmap snd tArgs'
      AppT eff tRet    = ctrType
      otherVars        = unapply ctrType
      quantifiedVars   = fmap PlainTV . nub
                                      $ mapMaybe freeVarName
                                                 (tArgs ++ otherVars)
                                          ++ [effs]
      memberConstraint = ConT ''Member `AppT` eff       `AppT` VarT effs
      resultType       = ConT ''Eff    `AppT` VarT effs `AppT` tRet

  return . SigD fnName
         . ForallT quantifiedVars [memberConstraint]
         . foldArrows
         $ tArgs ++ [resultType]
genSig _ _ = fail "genSig expects a GADT constructor"


------------------------------------------------------------------------------
-- | Gets the name of the free variable in the 'Type', if it exists.
freeVarName :: Type -> Maybe Name
freeVarName (VarT n) = Just n
freeVarName _ = Nothing


------------------------------------------------------------------------------
-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 (AppT . AppT ArrowT)


------------------------------------------------------------------------------
-- | Unfolds a type into any types which were applied together.
unapply :: Type -> [Type]
unapply (AppT a b) = unapply a ++ unapply b
unapply a = [a]

