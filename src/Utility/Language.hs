{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Utility.Language 
    ( LanguageContext (..)
    , isApplicable
    , isConsistent
    , isRebutting
    , isUndercutting
    , isAttack
    , equifinalPaths
    , equifinalPathSections
    , equifinalPathForQuery
    , concludedBy
    )where

import Control.Monad.Reader ( MonadIO, MonadReader, forM )
import           Env                  (App, Has (..), UseRuleOnly, grab)
import qualified Space.Language       as L
import qualified Space.Meta           as M (Negation(..), Imp(..), Name, rmdups, neg)
import qualified Data.HashMap.Strict as Map

-- |A strict rule is applicable with respect to a set of literals:
-- Names of the body of `arg1` is subet of Names of `arg2`
isApplicable :: L.Literal -> [L.Literal] -> Bool
isApplicable (L.Rule _ body _ _) ls =
    let
        s1 = L.name <$> body
        s2 = L.name <$> ls
    in and [ s `elem` s2 | s <- s1]
isApplicable (L.Atom _) _ = False

isConsistent :: L.Language -> Bool 
isConsistent literals = not . or $ auxiFunc literals []
    where 
        auxiFunc [_] acc = acc 
        auxiFunc (l:ls) acc = auxiFunc ls $ (M.negation l <$> ls)  ++ acc 

-- |TODO: Undermining need to be unified with rebutting 
-- BC algorithm sub argument is not included 
isRebutting :: L.Literal -> L.Literal -> Bool
isRebutting a b =
    let
        concA = L.conC a 
        concB = L.conC b
        isRebutting = M.negation concA concB
    in 
        isRebutting 
        && 
        M.D == L.imp b 

-- | `a` undercut `b` when 
-- 1. conclusion of `a` is the negation of argument `b`
-- 2. Toprule of `b` is defeasible rule `D`
-- TODO: now, undercut on Atom is allowed and will return False, 
-- 1. would this function cover all undercut situation described in paper? 
-- 2. Would this function cover situation other than described in paper ? for ex, Atom !?
isUndercutting :: L.Literal -> L.Literal ->  Bool
isUndercutting a b = 
    let 
        concA = L.conC a 
    in 
        M.negation concA b 
        && 
        L.imp b == M.D

isAttack :: L.Literal -> L.Literal -> Bool 
isAttack a b = a `isRebutting` b || a `isUndercutting` b


class Monad m => LanguageContext m where
    langMatchRuleFromAnony :: L.AnonyRule -> m L.Language
    langRuleAsConc :: L.Literal -> m L.Language
    langClosure :: L.Language -> m L.Language
    langPreferable :: L.Literal -> L.Literal -> m Bool 
    langAL :: L.Literal -> m L.Language 
    langASG :: L.Literal -> m L.Language 


-- | LanguageContext are functions that relies on Language (a set of Literal) implicitly.   \\ 
-- Detailed complexity in functions
instance LanguageContext App where
    langMatchRuleFromAnony = retriveRuleFromAnony
    langRuleAsConc = ruleAsConc
    langClosure = closure
    langPreferable = isPreferable
    langAL = rulesForLiteral
    langASG = rsForLiteral

-- | TODOs: This function need to be tested
-- How to read this FunctionContext Constrain together with Env ReaderT pattern.
-- The implementation below is not correct , fix it shortly after
isPreferable :: 
    ( MonadReader env m 
    , Has L.RdPrefMap env 
    , Has L.KnwlPrefMap env 
    , MonadIO m 
    ) => L.Literal -> L.Literal -> m Bool
isPreferable l1 l2 = do 
    prefMap1 <- grab @L.RdPrefMap
    prefMap2 <- grab @L.KnwlPrefMap
    let 
        prefMap = Map.union (L.getRdPrefMap prefMap1) (L.getKnwlPrefMap prefMap2)
        l1Name = L.name l1 
        l2Name = L.name l2 
        prel1 = Map.lookup l1Name prefMap 
        prel2 = Map.lookup l2Name prefMap 
    case (>=) <$> prel1 <*> prel2 of 
        Nothing -> error $ 
            "Preference information is missing: either " 
            ++ l1Name 
            ++ " or " 
            ++ l2Name
        Just b -> pure b 

-- |  Work for Argumentation.undercutting : O(n)     
-- Toprule function of Argumenation space returns rule with no name (Anonymonus Rule)     
-- There fore we need to find out if this rule is a member of our `Language`    
-- Depends on `Language` of the `env`. 
retriveRuleFromAnony::
    ( MonadReader env m
    , Has L.Language env
    , MonadIO m )
    => L.AnonyRule -> m L.Language
retriveRuleFromAnony ar = do
    language <- grab @L.Language
    pure $ auxi language ar
    where
        auxi (l:ls) ar =
            case l of
                r@L.Rule{} ->
                    if ar == L.AnonyRule r
                        then [r]
                        else auxi ls ar
                L.Atom{} -> auxi ls ar

-- | TR functions. O(n)    
-- Given a rule `r`::`L.Literal`,   
-- Returns all  `rules`::`L.Language` that conclude `r`.
ruleAsConc :: 
    ( MonadReader env m 
    , UseRuleOnly env
    , MonadIO m 
    )
    => L.Literal -> m L.Language 
ruleAsConc r = do 
    sRule <- L.getStrictRules <$> grab @L.StrictRules
    dRule <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    pure $ auxiFunc r (sRule ++ dRule) []
        where
            auxiFunc _ [] acc = acc
            auxiFunc (L.Atom _) _ _ = []
            auxiFunc r@(L.Rule n _ _ _) (l:ls) acc =
                if n == (L.name . L.conC $ l)
                    then auxiFunc r ls (l:acc)
                    else auxiFunc r ls acc

-- | Construct closure of a set of Literal `p`. O(n^2)    
-- Based on inital `p`     
-- Find all `rules` isApplicable on `p` and get corresponding conclusion `Cs`    
-- Add `rules`, `Cs` in `p` to get `newP`     
-- run closure `newP` again.     
-- Stop when `p` == `newP`    
closure :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m
    )=> [L.Literal] -> m [L.Literal]
closure p = do 
    sRules <- L.getStrictRules <$> grab @L.StrictRules
    let 
        heads = L.conC <$> sRules 
        bodys = concat $ L.body <$> sRules 
        closurePS = M.rmdups $ heads ++ bodys ++ p 
    pure closurePS
    
-- | Recursively find all Rules whose conclusion (head) are the given literal. O(n^2)\\
-- And find all rules whose conclusion are body's of these rules found above.\\\
-- And continue above step.     
-- This actually compute the support path of a given `Literal` .\\
-- It is different from GRI, because support path in GRI only compute support path for rule.\\\
-- It is different from `Definition 5`, because `Definition 5` has no recursive operation. \\
-- It is the same as described in the pseudo-code of `AL`.\\
-- This is the core of implementation of `BC.funcAL`.
rulesForLiteral ::
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => L.Literal -> m L.Language
rulesForLiteral l = do 
    sRules <- L.getStrictRules <$> grab @L.StrictRules
    dRules <- L.getDefeasibleRules <$> grab @L.DefeasibleRules
    pure $  M.rmdups $ accBodys [l] (sRules ++ dRules) [] []
    where
        accBodys [] _ _ acc = acc
        accBodys (ll:ls) lSpace seen acc =
            if
                ll `elem` seen then accBodys ls lSpace seen acc
                else
                    let tmpR = [ r | r <-lSpace , L.conC r == ll] 
                        tmpLit = concat (L.body <$> tmpR) ++ ls
                    in accBodys tmpLit lSpace (ll : seen) (tmpR ++ acc)

-- | Line 1-16 of ASG  
-- `rebutting` relies on `AL` to get all support path \\
-- `undercutting` relies on `neg` of previous result to get all negative connected rules.\\
-- `OneMoreStepAL` relies on `AL` to get all connected rules.\\
-- Sum these together and repeat again, untill no more rule are included.
rsForLiteral :: 
            ( MonadReader env m 
            , UseRuleOnly env 
            , MonadIO m 
            ) => L.Literal -> m L.Language
rsForLiteral l = do 
    rules4l <- rulesForLiteral l 
    asg rules4l 
    where 
        asg initRL = do 
                rebutting <- M.rmdups . concat <$> forM ( M.neg . L.conC <$> initRL) rulesForLiteral
                undercutting <- concat <$> forM (M.neg <$>  rebutting ++ initRL) ruleAsConc
                oneMoreStepAL <- M.rmdups . concat 
                                <$> forM 
                                    (initRL ++ rebutting ++ undercutting) 
                                    rulesForLiteral
                let all = M.rmdups $ initRL ++ oneMoreStepAL ++ undercutting ++ rebutting
                if all == initRL 
                    then pure all
                    else asg all 

-- | sRules conjunctively concluded some conclusion
equifinalPaths :: L.Language -> L.Language -> L.EquifinalPaths
equifinalPaths lang sRules = 
    let bodies = concat $ L.body <$> sRules
    in 
        if null bodies
            then [[sRules]]
        else 
            let equifinality = equifinalPathSections lang bodies  
            in
                do 
                    b <-  concat $ equifinalPaths lang <$> equifinality 
                    pure $ sRules : b



-- | `PathSection` is list of `Rules` between two level of arguments
-- `Path` is a list of `PathSection` consequentially connect facts with target arguments. 
-- `Equifinal Paths` is a list of `Path`s conclude the same argument based on different facts.
-- `lang`: env rule space
-- `bodies` : bodies that concluded by lower level rules.
-- `return`: equifinal path sections from lower-level rules to input bodies.[ path1, path2, path3 ...]
equifinalPathSections :: (Foldable t, Functor t) => [L.Literal] -> t L.Literal -> L.EquifinalPathSections
equifinalPathSections lang bodies =  
    let subLevel = concludedBy lang <$> bodies 
    in foldr createParallel [[]] subLevel 
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                path <- paths
                a <- ls 
                pure $  path:a 

concludedBy :: L.Language -> L.Literal -> L.Language
concludedBy lang l = [r | r <- lang , L.conC r == l]

-- | Given a Atom Literal , find all Equifinal Paths that conclude this Literal 
-- | TODOs: 
-- There is no sorting of these Equifinal Paths. The one with least rules should be placed in the front of the list. 
-- The sorting method should be configurable. 
equifinalPathForQuery :: L.Language -> L.Literal -> L.EquifinalPaths
equifinalPathForQuery lang conC= 
    let 
        dos = (:[]) <$> concludedBy lang conC
    in concat $ equifinalPaths lang <$> dos


evenLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
evenLayer lang pMap (p:ps) = 
    let
        nextLayerAttackers = queryNextLayerAttackers lang pMap p
        defeaders = oddLayer lang pMap <$> nextLayerAttackers
        succDefeaders = filter null defeaders 
    in if length succDefeaders == length nextLayerAttackers then p : concat defeaders else evenLayer lang pMap ps 
evenLayer lang pMap [] = []

-- | The purpose of this function is to guarantee that all input equifinal paths are defeaded.
--  `ps` is the equifinalPaths (Arguments)  that attack some upper layer argument(sub-argument)
-- 
oddLayer :: L.Language -> L.PreferenceMap -> L.EquifinalPaths -> L.EquifinalPaths
oddLayer lang pMap ps = 
    let 
        oddAttackers = queryNextLayerAttackers lang pMap <$> ps 
        succAttackers = filter null oddAttackers
    in 
        if 
            length succAttackers /= length ps 
            then [] 
            else  
                concat $ evenLayer lang pMap <$> concat oddAttackers

queryNextLayerAttackers :: L.Language -> L.PreferenceMap -> L.Path -> [L.EquifinalPaths]
queryNextLayerAttackers lang pMap p  = 
    let 
        conjunctiveRules = concat p 
        conjunctiveConcs = L.conC <$> conjunctiveRules 
    in queryNextLayerAttack lang pMap conjunctiveRules <$> conjunctiveConcs

-- | TODOs: 
-- What if to separate Equifinal Paths that disjunctively support `neg l`
-- However one of them has a circle ? 
-- 1. Change the name to undercut
queryRuleAttacks :: L.Language -> L.Literal -> L.EquifinalPaths
queryRuleAttacks lang l = equifinalPathForQuery lang $ M.neg l 

-- | `conC` is an conclusion from upper layer 
-- 1. get the path to `conC` (PathC), there is only one path, because it is a section of one equifinal path. 
-- 2. get equifinal paths to `neg conC`
-- 3. filter the paths of `neg conC` , select these can defeat (PathC).
-- returns: Arguments (Equifinal Paths) that successfully defeat `conC`.
-- TODOs: 
-- 1. Change the name : Attack in this case is defead as rebut here 
queryNextLayerAttack :: L.Language -> L.PreferenceMap -> L.Language -> L.Literal -> L.EquifinalPaths 
queryNextLayerAttack lang pMap pathRuls conC = 
    let
        argPath = getArgPath pathRuls conC 
        qConc = M.neg conC 
        attackPaths = equifinalPathForQuery lang qConc 
        attackerPaths = filter  (`defeat` argPath) attackPaths
    in attackerPaths 

-- | this is where we start compare two argument based on their rules composition. 
defeat :: L.Path -> L.Path -> Bool 
defeat = undefined 

-- | get a exactly path of an (sub) argument within a given path.  
getArgPath :: L.Language -> L.Literal -> L.Path 
getArgPath = undefined 

-- | TODO:
-- remove duplicated list of lists
-- In a 3 level list, remove duplicate list of lists , for example
{-
[ [[1],[1,2,3],[4,5]]
, [[2,3],[1,2,3]]
, [[1],[1,2,3],[4,5]]
]
-}
-- the 1st and 3rd element are the same, one should be removed
removeSndDup :: (Eq a) =>  [[[a]]] -> [[[a]]]
removeSndDup = undefined 


-- |  TODO:
-- In a 3 level list, remove elements that exists in previous lists, for example
{-
[ [[1],[1,2,3],[4,5]]
, [[2,3],[1,2,3]]
, [[1],[1,2,3,4],[4,5]]
]
-}
-- the 3rd array, 4 appears in [1,2,3,4] and [4,5], thus the 4 in [4,5] should be removed. 
removeExistingFstElem :: (Eq a) =>  [[[a]]] -> [[[a]]]
removeExistingFstElem = undefined 

-- | TODO: 
-- Convert a Path to individual Chain
-- A Chain of rules from Argument Conclusion to the fact
-- For each String, we could find at most one last defeasible rule. 
-- combine them together we have LastDefRules of a Path.
-- Different Chains could have same last defeasible rule, these duplication needs to be removed.
pathToChains:: L.Path -> [L.Language]
pathToChains = undefined 

-- | TODO:
-- Premises / Facts/ Grounds of a Path
pathToAxiomFacts:: L.Path -> L.Language 
pathToAxiomFacts = undefined 

pathToOrdinaryFacts:: L.Path -> L.Language 
pathToOrdinaryFacts = undefined 

chainToLastDefRule :: L.Language -> L.Literal
chainToLastDefRule = undefined 

chainToDefRules :: L.Language -> L.Language 
chainToDefRules = undefined 


-- | Strict: 
-- This implies rules must be strict ,however premiese could be ordinary.
isStrictPath :: L.Path -> Bool 
isStrictPath path = 
    let
        lang = concat path 
        rules = [p | p <- lang , not . null $ L.body p ]
    in and [L.imp p == M.S | p <- rules ]

-- | This argument relies on only Axiom premise
-- TODOs: Handle if there is no fact support this argument!
isFirmPath :: L.Path -> Bool 
isFirmPath path = 
    let 
        lang = concat path 
        premiese = [p | p <- lang , not . null $ L.body p ]
    in and [ L.imp p == M.S | p <- premiese]

type Orderings = L.PreferenceMap -> L.Language -> L.Language -> Bool 
type OrderingLink = L.PreferenceMap -> Orderings -> L.Path -> L.Path -> Bool 

eli :: L.PreferenceMap -> L.Path -> L.Path -> Bool 
eli pMap path1 path2 
        | null (concat path1) && null (concat path2) = False 
        | null (concat path1)  =  True 
        | eli' pMap (concat path2) (concat path1) = True 
        |  otherwise = False 
        

dem :: L.PreferenceMap -> L.Path -> L.Path -> Bool 
dem pMap path1 path2  
        | null (concat path1) && null (concat path2) = False 
        | null (concat path1)  =  True 
        | dem' pMap (concat path2) (concat path1) = True 
        |  otherwise = False 

eli' :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
eli' pMap l2 (l:ls)= undefined 
--     let 
--         rl = [ sl | sl <- l2 , preferThan pMap sl l]
--     in 
--          length rl /= length l2   || eli' pMap l2 ls 
-- eli' pMap l2 [] = False 

dem' :: L.PreferenceMap -> L.Language -> L.Language -> Bool 
dem' pMap (l:ls) l1 = undefined 
--     let 
--         rl = [ sl | sl <- l1 , preferThan pMap l sl]
--     in 
--          length rl == length l1   || dem' pMap ls l1 
-- dem' pMap l2 [] = False 


preferThan pMap l1 l2 = 
    let cMay = do 
                r1 <- Map.lookup (L.name l1) pMap 
                r2 <- Map.lookup (L.name l2) pMap 
                pure $ r1 >= r2 
    in Just True == cMay

---- aboves are 1st level functions
---- now is 2nd level functions that relies on them 

lastLink :: L.PreferenceMap -> Orderings -> L.Path -> L.Path -> Bool 
lastLink pm orderings pathA pathB 
    | null ldrA && null ldrB = orderings pm (axiA ++ ordiA) (axiB ++ ordiB)
    | otherwise = orderings pm ldrA ldrB
  where 
    ldrA :: L.Language
    ldrA = chainToLastDefRule <$> pathToChains pathA
    ldrB :: L.Language
    ldrB = chainToLastDefRule <$> pathToChains pathB
    axiA :: L.Language
    axiA = pathToAxiomFacts pathA 
    axiB :: L.Language
    axiB = pathToAxiomFacts pathB
    ordiA :: L.Language
    ordiA = pathToOrdinaryFacts pathA 
    ordiB :: L.Language
    ordiB = pathToOrdinaryFacts pathB

weakestLink :: L.PreferenceMap -> Orderings -> L.Path -> L.Path -> Bool 
weakestLink pm orderings pathA pathB 
    | isStrictPath pathA && isStrictPath pathB = orderings pm ordiA ordiB 
    | isFirmPath pathA && isFirmPath pathB = orderings pm drA drB
    | otherwise = orderings pm ordiA ordiB && orderings pm drA drB 
  where 
    drA :: L.Language
    drA = concat $ chainToDefRules <$> pathToChains pathA
    drB :: L.Language
    drB = concat $ chainToDefRules <$> pathToChains pathB
    ordiA :: L.Language
    ordiA = pathToOrdinaryFacts pathA 
    ordiB :: L.Language
    ordiB = pathToOrdinaryFacts pathB

-- | the successful path and related attacker paths
-- What we want from the query ? 
-- How to represent the Def relation. 
p :: L.EquifinalPaths -> OrderingLink -> Orderings 
p equiPaths oL = undefined 



-- | TODO:
-- For Last- link 

-- |TODO:
-- For weakest -link 
-- 1. Path is strict 
-- 2. Path is firm 
-- ect...

-- TODO: 
-- 1. map a path to a list of LastDefRules 
-- 2. map a path to a list of facts.
-- 3. necessary for weakest-link
--      3.1. check if a Path strict (no defeasible rules involved).
--      3.2. check if a Path firm (relies on only axiom premises)
----------------------------------------------
-- | Once dataset has been converted to Landspace
-- It would be not possible to has rules with no rule body.
-- 1. to check if there are loop support.
-- 2. to check if every conclusion has ground support. 
-- 3. Others:
validLanguageSpace
    :: L.Language
    -> Either L.Language L.Language
validLanguageSpace = undefined


scanOrderedSupportPath :: [[L.Literal]] -> ([L.Literal],Bool)
scanOrderedSupportPath = undefined 

getOrderedSupportPath :: L.Language -> M.Name -> [[L.Literal]]
getOrderedSupportPath = undefined 

tracer :: L.LanguageMap -> [L.Literal] -> Bool
tracer = undefined 

