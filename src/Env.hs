{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}


module Env where 

import Control.Monad.Reader 

import Space.Language (Language,Path, StrictRules(..),DefeasibleRules(..), PreferenceMap, RdPrefMap(..), KnwlPrefMap(..)) 
import Space.Argumentation (ArgumentationSpace ) 

-- | TODOs: 
-- 1. do we really need to wrap rules as StrictRules and DefeasibleRules respectively. 
-- 2. do we really need the envArguSpace here ? 
-- 3. a new PrefSpace is needed, this should be a function that given to literal and return true if
-- the first is at least prefer as the snd arg.

data Env = Env 
    { envLangSpace :: Language 
    , envSRuleSpace :: StrictRules
    , envDRuleSpace :: DefeasibleRules
    , envArguSpace :: ArgumentationSpace
    , envRdPrefMap:: RdPrefMap
    , envKnwlPrefMap :: KnwlPrefMap
    , envOrdering :: Order 
    } 

instance Show Env where 
    show env = 
        "LanguageSpace: " ++ show (envLangSpace env) ++ "\n" ++
        "Strict Rules: " ++ show (getStrictRules $ envSRuleSpace env) ++ "\n" ++ 
        "Defeasible Rules: " ++ show (getDefeasibleRules $ envDRuleSpace env) ++ "\n" ++ 
        "ArgumentationSpace: " ++ show (envArguSpace env) ++ "\n" ++ 
        "Preferrence Map of defeasible rules: " ++ show (getRdPrefMap $ envRdPrefMap env) ++ "\n" ++ 
        "Preferrence Map of axiom and ordinary knowledge: " ++ show (getKnwlPrefMap $ envKnwlPrefMap env) 

class Has field env where 
    obtain :: env -> field 

-- | Same type with different purpose should be wrapped respectively 
-- so that we they could be declare as instance of some class separately.
-- In this case `StrictRules` vs `DefeasibleRules` 
-- `D
instance Has Language Env where obtain = envLangSpace 
instance Has StrictRules Env where obtain = envSRuleSpace
instance Has DefeasibleRules Env where obtain = envDRuleSpace
instance Has ArgumentationSpace Env where obtain = envArguSpace 
instance Has RdPrefMap Env where obtain = envRdPrefMap
instance Has KnwlPrefMap Env where obtain = envKnwlPrefMap
instance Has Order Env where obtain = envOrdering

type UseRuleOnly env = (Has StrictRules env, Has DefeasibleRules env)
type OrderingContext env = (Has RdPrefMap env, Has KnwlPrefMap env)
type Order =  PreferenceMap -> Path -> Path ->  Bool 

grab :: forall field env m . (MonadReader env m , Has field env) => m field 
grab = asks $ obtain @field 

newtype App a = App 
    { unApp :: ReaderT Env IO a 
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader Env) 

runApp :: Env -> App a -> IO a 
runApp env app = (runReaderT $ unApp app) env 
