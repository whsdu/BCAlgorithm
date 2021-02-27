{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Utility.Argumentation
    ( ArgumentContext (..)
    , subArgument
    , topRule
    )where

import           Control.Monad.Reader
import qualified Space.Argumentation  as A (Argumentation (..), Preference (..),
                                            PreferenceSpace)
import qualified Space.Language       as L
import qualified Space.Meta           as M (Imp (..), Negation (..))

import           Env
import qualified Utility.Language     as UL (LanguageContext (..))



-- | Utility functions:
subArgument :: A.Argumentation -> [A.Argumentation]
subArgument a@(A.Argumentation _ [] _ _) = [a]
subArgument a@(A.Argumentation _ bs _ _) =
    let
        h = [a]
        t = concat $ subArgument <$> bs
    in h ++ t

topRule :: A.Argumentation -> L.AnonyRule
topRule (A.Argumentation _ bs i c) =
    let
        ruleBody = A.conC<$> bs
        anonyRule = L.Rule "" ruleBody i c
    in L.AnonyRule anonyRule

class Monad m => ArgumentContext m where
    argUndercutting :: A.Argumentation -> A.Argumentation -> m Bool
    argRebutting :: A.Argumentation -> A.Argumentation -> m Bool
    -- argPrefer :: A.Argumentation -> A.Argumentation -> m Bool
    -- argDefeat :: A.Argumentation -> A.Argumentation -> m Bool

instance ArgumentContext App where
    argUndercutting = undercutting
    argRebutting = rebutting
    -- argPrefer = preferable
    -- argDefeat = defeats

rebutting :: MonadIO m => A.Argumentation -> A.Argumentation -> m Bool
rebutting a b =
    let
        negConcA = M.neg . A.conC $ a
        bPrime = [b' | b' <- subArgument b, A.conC b' == negConcA]
    in  pure $
            not (null bPrime)
            &&
            M.D `elem` (A.imp <$> bPrime)

undercutting ::
    ( MonadReader env m
    , UL.LanguageContext m
    , MonadIO m)
    => A.Argumentation -> A.Argumentation -> m Bool
undercutting a b = do
    rules <- UL.langMatchRuleFromAnony (topRule b)
    let
        concA = A.conC a
        negNameTRB = M.neg <$> rules
    pure $ concA `elem` negNameTRB

-- preferable ::
--     ( MonadReader env m
--     , Has A.PreferenceSpace env
--     , MonadIO m
--     )
--     => A.Argumentation -> A.Argumentation -> m Bool
-- preferable a1 a2 = do
--     pf <- grab @A.PreferenceSpace
--     let
--         candiPrefer = A.Prefer a1 a2
--     pure $ or $ (==) candiPrefer <$> pf

-- defeats ::
--     ( MonadReader env m
--     , Has A.PreferenceSpace env
--     , UL.LanguageContext m
--     , MonadIO m)
--     => A.Argumentation -> A.Argumentation -> m Bool
-- defeats a b = do
--     isUndercutting <- undercutting a b
--     isPreferThan <- preferable a b
--     isRebutting <- rebutting a b
--     pure $ (isUndercutting || isRebutting) && isPreferThan
