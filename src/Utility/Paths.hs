{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}

{-
This is the rewire the module Path in the monad form. 
I was hoping this would make the logic more clear as which turns out to be more obscured ... ╮（￣▽￣）╭
at least now its easy to use .... ٩(*Ӧ)و
-}

module Utility.Paths where 

import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)
import Data.List (sort, reverse)
import  qualified Data.HashMap.Strict  as Map 

import Env ( grab, OrderingContext, UseRuleOnly,Has, Order) 

import qualified Space.Language as L 
import qualified Space.Meta as M 

import qualified Utility.Ordering  as O
import qualified Utility.Language as LU 

{-
EFP for single conclusion. 
-}
efp' :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.EquifinalPaths 
efp' conC = do
    cs <- concludeBy conC 
    let 
        dos = (:[]) <$> cs 
    rs <- mapM efp dos 
    pure $ sort $ concat rs 

-- | EFP  
-- Different from described in the paper, actual EFP is implemented on a list of rules. \\
-- Given a set of rules, the corresponding EFP is: \
-- 1. If bodies of these rules (R0) are all empty, there is only one path in this EFP and only one path section in this Path: R. 
-- 2. Else, get EFP-section that support these rules. Compute EFP of each `Ri` in this EFP-section. \\ 
-- The return paths in EFPs are actually same EFP to R0. They are not of different EFP, so we can concatenate them. 
efp :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Language -> m L.EquifinalPaths 
efp rules = do 
    let bodies = concat $ L.body <$> rules 
    if null bodies
        then pure [[rules]]
        else 
            do 
                equifinality <- efpSection bodies 
                rs <- mapM efp equifinality 
                pure $ 
                    do
                        r <- concat rs 
                        pure $ rules : r

-- | Auxiliary function of EFP
-- `bodies` is a list of proposition [a,b,c,d,...] \\
-- These proposition supported by list of lists of rules [Ra,Rb,Rc,Rd,...] \\
-- The core computation of this function is `foldr createParallel [[]] sublevel \\
-- `foldr createParallel` [[]] [[1,2],[4,5],[6]]  = [[1,4,6],[1,5,6],[2,4,6],[2,5,6]]
efpSection :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    , Traversable t 
    ) =>t L.Literal -> m L.EquifinalPathSections
efpSection bodies = do 
    subLevel <- mapM concludeBy bodies 
    pure $ foldr createParallel [[]] subLevel 
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                pa <- paths
                a <- ls 
                pure $  pa:a 

{-
The Rule Level Graph is being represented with a list of rules.
Following function server the purpose of:
1. Find a list of rules that eventually compose an argument. 
2. Find different lists of rules that equivalent to different arguments of the same conclusion.
-}
concludeBy :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.Language 
concludeBy l = do 
    dRules <- grab @L.DefeasibleRules
    sRules <- grab @L.StrictRules
    let rules = L.getDefeasibleRules dRules ++ L.getStrictRules sRules
    pure [r | r <- rules , L.conC r == l]


{- Backward Chaining Search
Given: 
1. The rule level graph representation (the List of Rules).
2. The prefernceMap that can be used to compute the preference relation between two arugment. 
find the 
-}

query :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    , UseRuleOnly env 
    ) => L.Literal -> m [L.EquifinalPaths]
query query = do
        dRules <- grab @L.DefeasibleRules
        sRules <- grab @L.StrictRules
        let unseen = L.getDefeasibleRules dRules ++ L.getStrictRules sRules
        efpQuery <- efp' query 
        evenLayer unseen efpQuery 

evenLayer ::
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    ) => L.Language -> L.EquifinalPaths -> m [L.EquifinalPaths]
evenLayer unseen (p:ps) = do
    attackers <- queryPathAttackers' unseen p 
    newUnseen <- removeSeenPath' p unseen 
    defenders <- mapM (oddLayer newUnseen ) attackers 
    let succDefeaders = filter (not . null) defenders
    if length succDefeaders == length attackers 
        then pure $ [p] : concat ( concat succDefeaders)
    else 
        evenLayer unseen ps 
evenLayer  _ [] = pure []

oddLayer :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    ) => L.Language -> L.EquifinalPaths -> m [[L.EquifinalPaths]]
oddLayer unseen efp = do 
    subAttackers <- mapM (queryPathAttackers' unseen) efp
    let
        attackPairs = zip efp subAttackers 
        attackGroups = [ attackPair | attackPair <- attackPairs , (not . null) (snd attackPair)]
        seen = fst <$> attackGroups
    if length attackGroups /= length efp 
        then pure [] 
        else 
            do 
            newUnseen <- removeSeenPath' (concat seen) unseen
            tt <- mapM (checkAttackGroup' newUnseen) attackGroups
            if 
                [] `notElem` tt 
                then 
                    pure tt 
                else 
                    pure [] 
    where 
         checkAttackGroup' :: 
            ( MonadReader env m 
            , MonadIO m 
            , OrderingContext env
            , Has Order env 
            ) => L.Language -> (L.Path ,[L.EquifinalPaths]) -> m [L.EquifinalPaths]
         checkAttackGroup' unseen (path, group) = do
                defenders <- mapM (evenLayer unseen) group 
                let 
                    defenderGroup = concat defenders 
                if not . null $ defenderGroup 
                    then pure $ [path] : defenderGroup
                    else 
                        pure []

queryPathAttackers' :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    ) => L.Language -> L.Path -> m [L.EquifinalPaths]
queryPathAttackers' unseen path =  do 
    nextLayerRebutting <- queryPathRebuts' unseen path
    nextLayerUndercutting <- queryPathUndercut' unseen path 
    pure $ filter ( not .null ) (nextLayerRebutting ++ nextLayerUndercutting)

queryPathRebuts' :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    ) => L.Language -> L.Path -> m [L.EquifinalPaths]
queryPathRebuts' unseen p  = do
    let 
        defeasible = [r | r <- concat p , L.imp r == M.D]
        conjunctiveConcs = L.conC <$> defeasible
    mapM (queryConcRebuts' unseen p) conjunctiveConcs

queryConcRebuts' :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    , Has Order env 
    ) => L.Language ->  L.Path -> L.Literal -> m L.EquifinalPaths 
queryConcRebuts' lang pathRuls conC = do 
    rdPreMap <- L.getRdPrefMap <$> grab @L.RdPrefMap 
    kwPreMap <- L.getKnwlPrefMap <$> grab @L.KnwlPrefMap 
    ordering <- grab @Order
    let 
        pMap = Map.union rdPreMap kwPreMap
        qConc = M.neg conC 
        argPath = head $ equifinalPathForQuery' (concat pathRuls) conC  -- This part do not need to lift to monad level
        attackPaths = equifinalPathForQuery' lang qConc 
    pure  [p | p <- attackPaths, ordering pMap p argPath] -- This is problematic to convert to monad level 

queryPathUndercut' :: 
    ( MonadIO m
    , MonadReader env m  
    ) =>L.Language -> L.Path -> m [L.EquifinalPaths]
queryPathUndercut' lang p = do 
    let 
        defeasibleRules = [r | r <- concat p, L.imp r == M.D && (not . null) (L.body r)]
        undercutted = M.neg <$> defeasibleRules
    pure $ equifinalPathForQuery' lang <$> undercutted 

removeSeenPath' :: 
    ( MonadIO m
    , MonadReader env m  
    )=> L.Path -> L.Language -> m L.Language
removeSeenPath' path lang = do 
    let 
        seen = concat path 
    pure  [l | l <-lang , l `notElem` seen]

equifinalPathForQuery' :: L.Language -> L.Literal -> L.EquifinalPaths
equifinalPathForQuery' lang conC= 
    let 
        dos = (:[]) <$> concludedBy' lang conC
    in sortEquifinalPaths' . concat $ equifinalPaths' lang <$> dos
    where 
        concludedBy' :: L.Language -> L.Literal -> L.Language
        concludedBy' lang l = [r | r <- lang , L.conC r == l]
        -- | efpSection
        equifinalPathSections' :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> L.EquifinalPathSections
        equifinalPathSections' lang bodies =  
            let subLevel = concludedBy' lang <$> bodies 
            in foldr createParallel [[]] subLevel 
            where 
                createParallel :: [a] -> [[a]] -> [[a]] 
                createParallel paths ls = do 
                        path <- paths
                        a <- ls 
                        pure $  path:a 

        -- | efp 
        equifinalPaths' :: L.Language -> L.Language -> L.EquifinalPaths
        equifinalPaths' lang sRules = 
            let bodies = concat $ L.body <$> sRules
            in 
                if null bodies
                    then [[sRules]]
                else 
                    let equifinality = equifinalPathSections' lang bodies  
                    in
                        do 
                            b <-  concat $ equifinalPaths' lang <$> equifinality 
                            pure $ sRules : b

        sortEquifinalPaths' :: L.EquifinalPaths -> L.EquifinalPaths 
        sortEquifinalPaths' paths = 
            let 
                pathLengths = length <$> paths 
                sortedLength = sort pathLengths
            in [p | l <- sortedLength, p <- paths, length p == l]




-- -- | select one path from the list of equifinal paths.
-- -- If no one attacks this path, return this path
-- -- If it is attacked by some other equifinalpaths:
-- --    check if it is defeated
-- --    if this path is not defeated by any one, return this path together with all next-level equifinal paths that attack it.
-- -- otherwise check the next path in the list of equifinal paths. 
-- -- If no path in the list of equifinal paths pass the check, return empty []. 
-- -- `attackGroup` is of type [EquifinalPaths], each EquifinalPaths attack certain part of the same path.
-- evenLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
-- evenLayer' lang pMap (p:ps) = 
--     let
--         attackGroup = queryPathAttackers lang pMap p 
--         newLang = removeSeenPath p lang 
--         defenders = oddLayer' newLang pMap <$> attackGroup
--         succDefeaders = filter (not . null) defenders
--     in if length succDefeaders == length attackGroup 
--         then [p] : concat ( concat succDefeaders)
--         else evenLayer' lang pMap ps 
-- evenLayer' _ _ [] = []
-- 
-- 
-- 
-- -- | every path in this list of equifinal paths should be defeated 
-- -- If all these paths are defeated then return these path together with the next layer defeaders path 
-- -- If not all these path are dfeated then return [].
-- -- oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [L.EquifinalPaths]
-- oddLayer' :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> [[L.EquifinalPaths]]
-- oddLayer' lang pMap ps = 
--     let 
--         attackGroups = [ (p, queryPathAttackers lang pMap p) | p <- ps , (not . null) (queryPathAttackers lang pMap p)]
--     in if length attackGroups /=  length ps         -- if not every paths in this EFP are attacked by some one then return empty.
--         then [] 
--         else 
--             let 
--                 seen = [ p | p <- ps , (not . null) (queryPathAttackers lang pMap p)]
--                 newLang = removeSeenPath (concat seen) lang 
--             -- in checkAttackGroup newLang pMap <$> attackGroups
--                 tt = checkAttackGroup newLang pMap <$> attackGroups -- 
--             in 
--                 if [] `notElem` tt then tt else [] 
--     where 
--         checkAttackGroup :: L.Language -> L.PreferenceMap -> (L.Path ,[L.EquifinalPaths]) -> [L.EquifinalPaths]
--         checkAttackGroup lang fMap (path, group) = 
--             let 
--                 defenderGroup = concat (evenLayer' lang fMap <$> group)
--             in 
--                 if not . null $ defenderGroup  then [path] : defenderGroup else [] 
-- 
-- removeSeenPath :: L.Path -> L.Language -> L.Language
-- removeSeenPath path lang = 
--     let 
--         seen = concat path 
--     in [l | l <-lang , l `notElem` seen]
-- 
-- sortEquifinalPaths :: L.EquifinalPaths -> L.EquifinalPaths 
-- sortEquifinalPaths paths = 
--     let 
--         pathLengths = length <$> paths 
--         sortedLength = sort pathLengths
--     in [p | l <- sortedLength, p <- paths, length p == l]
-- 
-- -- | [DEF] =  [DEF(path, p')| p' <- L, DEF(path, p') not null ] \  
-- -- `path` can be attacked at more than one target. \  
-- -- Therefore, it is possible that there are more than one EFP that defeat `path`.  \   
-- -- This function return these EFPs. \\
-- -- return : [DEF] \\
-- -- TODO: the (not . null) maybe not necessary here. 
-- queryPathAttackers :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
-- queryPathAttackers lang pMap path = 
--     let 
--         nextLayerRebutting = queryPathRebuts' lang pMap path
--         nextLayerUndercutting = queryPathUndercut' lang path 
--     in filter ( not .null ) (nextLayerRebutting ++ nextLayerUndercutting)
-- 
-- {-Next 3 functions are auxiliary functions of queryPathAttackers (DEF) -}
-- 
-- -- | [EFP]=  [EFP (p')| p' <- L, p <- path, imp(p) = '=>' , body(p) isn't null, p' = !p] \  
-- -- `path` can be undercuted at more than one target.  \   
-- -- All EFPs that undercut `path`.
-- queryPathUndercut' :: L.Language -> L.Path -> [L.EquifinalPaths]
-- queryPathUndercut' lang p = 
--     let 
--         defeasibleRules = [r | r <- concat p, L.imp r == M.D && (not . null) (L.body r)]
--         undercutted = M.neg <$> defeasibleRules
--     in  equifinalPathForQuery' lang <$> undercutted 
-- 
-- -- | [EFP]=  [EFP (c')| c' <- L, p <- path, c <- body(p), imp(p) = '=>' , c' = !c] \ 
-- -- `path` can be attacked at more than one undercut.  \  
-- -- All EFPs that undercut `path`.
-- queryPathRebuts' :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
-- queryPathRebuts' lang pMap p  = 
--     let 
--         defeasible = [r | r <- concat p , L.imp r == M.D]
--         conjunctiveConcs = L.conC <$> defeasible
--     in queryConcRebuts' lang pMap p <$> conjunctiveConcs
-- 
-- -- | Given conclusion c
-- -- Get all Equifinal paths of neg c
-- -- select paths that defeat path of c.
-- -- TODO: 
-- -- Ordering functions is hard coded here. 
-- queryConcRebuts' :: L.Language -> L.PreferenceMap -> L.Path -> L.Literal -> L.EquifinalPaths 
-- queryConcRebuts' lang pMap pathRuls conC = 
--     let
--         qConc = M.neg conC 
--         argPath = head $ equifinalPathForQuery' (concat pathRuls) conC  -- This part do not need to lift to monad level
--         attackPaths = equifinalPathForQuery' lang qConc 
--     in [p | p <- attackPaths, O.weakestLink pMap O.dem p argPath] -- This is problematic to convert to monad level 
-- 
-- 
-- 
-- {- Following code is the original implementation for 
-- 1. developing purpose.
-- 2. simplify the code structure. 
-- TODO: These functions are redundant , remove in future. 
-- -}
-- 
-- equifinalPathForQuery' :: L.Language -> L.Literal -> L.EquifinalPaths
-- equifinalPathForQuery' lang conC= 
--     let 
--         dos = (:[]) <$> concludedBy' lang conC
--     in sortEquifinalPaths . concat $ equifinalPaths' lang <$> dos
-- 
-- concludedBy' :: L.Language -> L.Literal -> L.Language
-- concludedBy' lang l = [r | r <- lang , L.conC r == l]
-- 
-- -- | efpSection
-- equifinalPathSections' :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> L.EquifinalPathSections
-- equifinalPathSections' lang bodies =  
--     let subLevel = concludedBy' lang <$> bodies 
--     in foldr createParallel [[]] subLevel 
--     where 
--         createParallel :: [a] -> [[a]] -> [[a]] 
--         createParallel paths ls = do 
--                 path <- paths
--                 a <- ls 
--                 pure $  path:a 
-- 
-- -- | efp 
-- equifinalPaths' :: L.Language -> L.Language -> L.EquifinalPaths
-- equifinalPaths' lang sRules = 
--     let bodies = concat $ L.body <$> sRules
--     in 
--         if null bodies
--             then [[sRules]]
--         else 
--             let equifinality = equifinalPathSections' lang bodies  
--             in
--                 do 
--                     b <-  concat $ equifinalPaths' lang <$> equifinality 
--                     pure $ sRules : b