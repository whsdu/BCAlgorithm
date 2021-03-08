{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Utility.Ordering where 

import qualified Space.Meta as M 
import qualified Space.Defeasible as L 

import EnvDef 

import qualified Data.HashMap.Strict as Map 
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO)

-- isPreferableThan ::
--     ( MonadReader env m 
--     , OrderingContext env
--     , MonadIO m 
--     ) => L.Path -> L.Path -> m Bool 
-- isPreferableThan a b = undefined 

lastEli :: L.PreferenceMap  -> L.Path -> L.Path -> Bool 
lastEli = lastLink eli 

lastDem :: L.PreferenceMap ->  L.Path -> L.Path -> Bool 
lastDem = lastLink dem 

weakestEli :: L.PreferenceMap  -> L.Path -> L.Path -> Bool 
weakestEli = weakestLink eli 

weakestDem :: L.PreferenceMap  -> L.Path -> L.Path -> Bool 
weakestDem = weakestLink dem 

lastEliM :: ( MonadReader env m , OrderingContext env, MonadIO m ) => L.Path -> L.Path -> m Bool 
lastEliM argA argB = do 
    prefMap <- getPreferMap
    pure $ lastLink eli prefMap argA argB 

lastDemM ::( MonadReader env m , OrderingContext env, MonadIO m ) => L.Path -> L.Path -> m Bool 
lastDemM argA argB =  do 
    prefMap <- getPreferMap
    pure $ lastLink dem prefMap argA argB

weakestEliM :: ( MonadReader env m , OrderingContext env, MonadIO m ) => L.Path -> L.Path -> m Bool 
weakestEliM argA argB = do 
    prefMap <- getPreferMap
    pure $ weakestLink eli prefMap argA argB

weakestDemM :: ( MonadReader env m , OrderingContext env, MonadIO m ) => L.Path -> L.Path -> m Bool 
weakestDemM argA argB = do 
    prefMap <- getPreferMap
    pure $ weakestLink dem prefMap argA argB

{- Auxiliary function help to union preference map of defeasible rule and ordinary premises-}

-- | check if `argA` is preferable than `argB`
-- TODOs: maybe could move: 
-- 1. Links type 
-- 2. Ordering type 
-- to env 
type Orderings = L.PreferenceMap -> L.Language -> L.Language -> Bool 
type MonadOrderings = forall m . MonadIO m => L.Path -> L.Path -> m Bool 

getPreferMap :: 
    ( MonadReader env m 
    , OrderingContext env
    , MonadIO m 
    ) => m L.PreferenceMap
getPreferMap = do 
    rdPreferenceMap <- L.getRdPrefMap <$> grab @L.RdPrefMap
    knPreferenceMap <- L.getKnwlPrefMap <$> grab @L.KnwlPrefMap
    let prefMap = Map.union rdPreferenceMap knPreferenceMap
    pure prefMap 


-- | Last-Link : select particular set of components of two Arguments(Paths)
-- Check the preferable relation with certain method (dem or eli)
lastLink :: Orderings -> L.PreferenceMap -> L.Path -> L.Path -> Bool 
lastLink orderings pm argA argB
    | null ldrA && null ldrB = orderings pm (axiA ++ ordiA) (axiB ++ ordiB)
    | null ldrA = True 
    | otherwise = orderings pm ldrA ldrB
  where 
    ldrA = pathToLDR argA
    ldrB = pathToLDR argB
    axiA = pathToAxiomFacts argA 
    axiB = pathToAxiomFacts argB
    ordiA = pathToOrdinaryFacts argA 
    ordiB = pathToOrdinaryFacts argB

weakestLink :: Orderings ->  L.PreferenceMap ->  L.Path -> L.Path -> Bool 
weakestLink orderings pm pathA pathB 
    | isStrictPath pathA && isStrictPath pathB = orderings pm ordiA ordiB 
    | isFirmPath pathA && isFirmPath pathB = orderings pm drA drB
    | otherwise = orderings pm ordiA ordiB && orderings pm drA drB 
  where 
    drA = pathToDefRules pathA
    drB = pathToDefRules pathB
    ordiA = pathToOrdinaryFacts pathA 
    ordiB = pathToOrdinaryFacts pathB

-- | Eli & Dem : Method that aggregate preference inform from Liter 
-- Check the preferable relation between two sets of argument component
-- If Argument A is preferable than Argument B
eli :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
eli pMap argA argB 
        | null argA && null argB = False 
        | null argB = False 
        | null argA  =  True 
        | otherwise = eli' pMap argA argB
        

dem :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
dem pMap argA argB
        | null argA && null argB = False 
        | null argB = False 
        | null argA  =  True 
        | otherwise = dem' pMap argA argB
        
        

eli' :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
eli' pMap l1 (l:ls)= 
    let 
        rl = [sl | sl <- l1 , preferThan pMap sl l]
    in
        ((length rl == length l1) || eli' pMap l1 ls)
eli' pMap l1 [] = False 

dem' :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
dem' pMap (l:ls) l2 =
    let 
        rl = [sl | sl <- l2, preferThan pMap l sl]
    in 
        ((length rl == length l2) || dem' pMap ls l2)
dem' pMap [] l2 = False 

preferThan pMap l1 l2 = 
    let cMay = do 
                r1 <- Map.lookup (L.name l1) pMap 
                r2 <- Map.lookup (L.name l2) pMap 
                pure $ r1 >= r2 
    in Just True == cMay

{- Two auxiliary functions being used for Last-Link only -}

pathToAxiomFacts:: L.Path -> L.Language 
pathToAxiomFacts path = 
    let 
        rules = concat path
    in [r | r <- rules , L.imp r == M.S &&  null (L.body r)]



pathToLDR:: L.Path -> L.Language
pathToLDR path = 
    let 
        rules = concat path 
    in getLDR rules (head rules)
    where 
        getLDR :: L.Language -> L.Literal-> L.Language 
        getLDR rules r
            | L.imp r == M.D = [r]
            | otherwise =  concat $ getLDR rules <$> [subr | subr <- rules, L.conC subr `elem` L.body r ]

{-Two auxiliary functions being used for both Last-Link and Weakest-Link-}

pathToOrdinaryFacts:: L.Path -> L.Language 
pathToOrdinaryFacts path = 
    let 
        rules = concat path
    in [r | r <- rules , L.imp r == M.D && null (L.body r)]

pathToDefRules :: L.Path -> L.Language 
pathToDefRules path = 
    let 
        rules = concat path 
    in [r | r <- rules, L.imp r == M.D && (not . null) (L.body r)]

{-Two auxiliary functions being used for both Weakest-Link Only-}

-- |  This argument relies on only strict rules (premise could be ordinary). 
-- TODOs: Exception Handling: When there is no fact support.
isStrictPath :: L.Path -> Bool 
isStrictPath path = 
    let
        lang = concat path 
        rules = [p | p <- lang , not . null $ L.body p ]
    in and [L.imp p == M.S | p <- rules ]

-- | This argument relies on only Axiom premise
-- TODOs: Exception handling: When there is no fact support.
isFirmPath :: L.Path -> Bool 
isFirmPath path = 
    let 
        lang = concat path 
        premiese = [p | p <- lang , null $ L.body p ]
    in and [ L.imp p == M.S | p <- premiese]