{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Algorithm.BC where

import Control.Monad.Reader 

import qualified Space.Language as L 
import qualified Space.Meta as M 

import qualified Utility.Language as LU
import Env 

-- | Rely on Language Context function langAL      
-- `L.Literal` is the query.          
-- `L.Language` is the `A` ***(argumentation set)*** described in paper.
funcAL ::
    ( LU.LanguageContext m 
    ) => L.Literal -> m L.Language
funcAL = LU.langAL

-- | Rely on Language Context function langASG
-- `L.Literal` is the query.          
-- `L.Language` is the `A` ***(argumentation set)*** described in paper.
funcASG :: 
    ( LU.LanguageContext m 
    ) => L.Literal -> m L.Language
funcASG = LU.langASG

-- | This is actually finished the def-generate and as-filter in one go.     
-- This function also relies on `LanguageContext` function to get access to Preference.  
-- The filtration is not being recorded.
funcDefGen :: 
    ( MonadReader env m
    , Has L.PreferenceMap env 
    , LU.LanguageContext m 
    , MonadIO m ) => L.Language -> m (L.Language, [(L.Literal,L.Literal)])
funcDefGen = undefined 
-- funcDefGen language = do
    -- perfs <- grab @L.PreferenceMap
    -- let
        -- isPreferable = LU.isPreferable perfs 
        -- def = do 
            -- la <- language 
            -- lb <- language 
            -- guard $ la `LU.isAttack` lb 
            -- guard $ la `isPreferable` lb 
            -- pure $ L.Preference la lb 
    -- pure  ( language, def )