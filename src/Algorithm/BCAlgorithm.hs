{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Algorithm.BCAlgorithm where 

import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map 
import qualified Space.Defeasible as D
import qualified Utility.Defeasible as MD
import qualified Space.Meta as M 
import EnvDef  
import Space.DefeasibleInstance ( Conflict(..) )
import Space.DefeasibleFrame ( Attack(conflict) )

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad.Reader 
import Control.Monad.IO.Class 


type Base = D.Argument 

data ArgumentStatus = Warranted | Unwarranted 

data Defeater = SW D.Path | Node [(D.Path,Defeater)] deriving (Show)

type SearchRecord = (D.Path,Defeater)
type SearchRecords = [SearchRecord]

type PathRecord = (D.Path,D.Argument) 
type PathRecords = [PathRecord]

data Board = Board {lucky :: SearchRecords , waiting :: PathRecords, futile :: SearchRecords, seen :: D.Language} deriving(Show)

query:: 
    ( Has D.Language env  
    , UseRuleOnly env 
    , MonadReader env m
    , MonadIO m 
    , OrderingContext env
    ) => D.Argument ->  m Defeater 
query incArgu = queryArgument incArgu []

-- | rewrite query 
queryArgument :: 
    ( Has D.Language env  
    , UseRuleOnly env 
    , MonadReader env m
    , MonadIO m 
    , OrderingContext env
    ) => D.Argument -> D.Language ->  m Defeater 
queryArgument incArgu accSeen = do 
    let 
        tmpBoard = initialBoard incArgu 
        initBoard = tmpBoard{seen=accSeen}
    defeatChain initBoard 

-- | rewrite of chain construction 
defeatChain :: 
    ( Has D.Language env  
    , UseRuleOnly env 
    , MonadReader env m
    , MonadIO m 
    , OrderingContext env
    ) => Board -> m Defeater
defeatChain step1Board = do 
    step1 <- defeatDetection step1Board 
    liftIO $ print step1
    case step1 of 
        Right p -> pure $ warranted p 
        Left step2Board -> do 
            step3 <- pathSelection step2Board 
            case step3 of 
                Just (base, step3Board) ->  do 
                    luckyExtend <- aguConstruction base 
                    let 
                        step3Lucky = lucky step3Board
                        step4Board = step3Board{lucky=step3Lucky++luckyExtend}
                    defeatChain step4Board
                Nothing -> do 
                    step6 <- defeaterChecker step2Board 
                    case step6 of 
                        Right unw -> pure unw 
                        Left step6Board -> defeatChain step6Board


{- 1
-- Intialize a given set of rules (this set of rules concludes to a set of proposition , old equifinal path section) 
as an incomplete-argument (incArgument). 
        - Initialize these in-complete paths SearchRecord == 'lucky' (each SearchRecord has a Defeater as 'Node[]')
        - Initialize empty PathRecords      [] == 'waiting' 
        - Initialize empty SearchRecords    [] == 'futile'  
Notes: 
    1. incArgu is of type Argument = [Paths,...,Path] 
    2. Each Path in [Paths,...,Path] is a head of an incomplete-path.
    3. Path in [Paths,...,Path] are all [Rules], and Rules has same set of heads. 
-}
-- go to 2 
initialBoard :: D.Argument -> Board 
initialBoard incArgu = 
    let 
        luckySet = (,Node[]) <$> incArgu 
    in Board luckySet [] [] []

{- 2
- Given Board from step 1
    - Check 'lucky': If any unseen inc-defeater found for each path: 
        - include all newly defeated target to seen
        - Attaches the inc-defeater to related Path , move this PathRecord to 'waiting'
        - If path has old related defeater(much be unwarranted defeater), then remove this old defeater and attach this new in-defeater. 
            - a SearchRecord turns to be a PathRecord
    - Check if any SearchRecord in 'lucky' contains complete Path. 
        - Yes: go to 5 
            - Exit : Pass the complete Path Record to 5 and get return result 
        - No : go to 3 
            - Continue construction
Notes:
    1. when detecting one search record in lucky has new in-defeater, remove old fail-defeater, then put to tmpWaiting
        inc-Argument defeat path
    2. select one lucker from possibly many in lucky
-}
defeatDetection :: 
    ( Has D.Language env  
    , UseRuleOnly env 
    , MonadReader env m
    , MonadIO m 
    , OrderingContext env 
    ) => Board -> m (Either Board SearchRecord)
defeatDetection board@Board{..} = do 
        (newWaiting, newSeen, newLucky) <- checkLuckySet seen lucky
        let 
            completeLuckers = checkLuckyComplete newLucky 
        if (not . null) completeLuckers 
            then pure $ Right $ selectOneLucker completeLuckers 
            else pure $ Left $ board{lucky=newLucky, waiting=waiting ++newWaiting, seen =newSeen}

{- 3
- Given Board from step 2
    - If 'lucky' contains more than one SearchRecord, select One (Path selection strategy)
        - Set this one as 'Base' :: SearchRecord 
        - Move this SearchRecord out of lucky 
        - go to 4 == Just SearchRecord
    - If 'lucky' is empty 
        - go to 6 == Nothing 
-}
pathSelection ::
    ( MonadReader  env m
    , MonadIO m
    )=> Board -> m (Maybe (SearchRecord, Board))
pathSelection board@Board{..} = case lucky of 
    [] -> pure Nothing 
    _ ->  do 
        (base, newlucky) <- selectionOne lucky 
        let 
            newBoard = board{lucky=newlucky}
        pure $ Just (base,newBoard)

selectionOne :: 
    ( MonadReader env m 
    , MonadIO m
    )=> SearchRecords -> m (SearchRecord, SearchRecords)
selectionOne srs = do 
    let 
        sortByLength = sortBy (flip compare `on` (length . fst)) srs 
        sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) srs 
    pure (head sortByDefeasible, tail sortByDefeasible)  
    where 
        getDef :: D.Path -> D.Language 
        getDef p = 
            let 
                rules = concat p 
            in [r | r<-rules, D.imp r == M.D]

{- 4
- Given SearchRecord selected from step 3, continue construction. 
    - More path may be discovered, thus append existing Defeater to all new paths. 
    - These new paths are New SearchRecords.
- Get a new 'lucky' = New SearchRecords + old lucky (SearchRecords). 

- get New Board with new 'lucky' from Board in step 3 
- go to step 2 (input new Board)
-}
aguConstruction ::
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => SearchRecord -> m SearchRecords
aguConstruction (p,defeater) = do 
    tmpArg <- pathExtend p 
    pure $ (,defeater) <$> tmpArg 




{- 5 : Defeater Return 
- Given a complete path record :: PathRecord. 
    - if SearchRecord related argument is Node [] . 
        - return SW Path 
    - else 
        - return Node [(Path, Defeater)]
        - convert SearchRecord :: (Path, Defeater) to Node [(Path, Defeater)]
        - In this case, existing Defeater must be a non-warranted argument (Saved from waiting). 
-}
warranted :: SearchRecord -> Defeater 
warranted (p, Node []) = SW p 
warranted record = Node [record]

{- 6 
Known 'lucky' is empty  from step 3
- Select a PathRecord from 'waiting' according to Step 10
- The purpose is to check whether the defeater is Warranted. 
    - Construct the defeater and check defeater return , if the defeater is Warranted . 
        - go to 7 
    - If 'waiting' is empty because of step 7 and step 9
        - go to  step 8 
    - Construct the defeater and check defeater return, if the Defeaters is UnWarranted. 
        - go to 9 
-}
defeaterChecker ::
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env 
    , UseRuleOnly env 
    , Has D.Language env 
    ) => Board -> m (Either Board Defeater)
defeaterChecker board@Board{..} = 
    case waiting of 
        [] -> pure $ Right $ unwarranted futile 
        _ -> do 
            ((p,incArgument), tmpWaiting) <- selectionTwo waiting 
            result <- queryArgument incArgument seen 
            case tellQuery result of 
                Warranted ->  do
                    let 
                        newBoard = waitingToFutile (p,result) tmpWaiting board
                    defeaterChecker newBoard 
                Unwarranted -> do 
                    let 
                        newBoard = survived (p,result) tmpWaiting board
                    pure $ Left newBoard 

selectionTwo :: 
    ( MonadReader env m 
    , MonadIO m
    )=> PathRecords -> m (PathRecord, PathRecords)
selectionTwo prs = do 
    let 
        sortByLength = sortBy (flip compare `on` (length . fst)) prs 
        sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) prs 
    pure (head sortByDefeasible, tail sortByDefeasible)  
    where 
        getDef :: D.Path -> D.Language 
        getDef p = 
            let 
                rules = concat p 
            in [r | r<-rules, D.imp r == M.D]
            
tellQuery :: Defeater -> ArgumentStatus 
tellQuery (SW _) = Warranted
tellQuery (Node [x]) = Warranted 
tellQuery (Node _) = Unwarranted
{- 7
In step 6, given the defeater is Warranted. We have a new SearchRecord  
    - Remove old PathRecord from 'waiting'
    - Move new SearchRecord  to 'futile'
        - go back to 6 
-}
-- TODO: futile should be [(Path, Defeater)]
waitingToFutile :: SearchRecord -> PathRecords -> Board -> Board
waitingToFutile newFutile newWaiting oldBoard@Board{..} = oldBoard{waiting=newWaiting, futile=newFutile:futile} 

{- 8 
Given 'lucky' and 'waiting' are all empty.  
Then all defeater are warranted. In this case
    - convert futile :: [(Path,Defeater)] to Node [(Path,Defeater)]
-}
unwarranted :: SearchRecords -> Defeater 
unwarranted = Node 

{- 9 
Given that the PathRecord related defeater is unwarranted. We have a new SearchReacord
    - Remove old PathRecord from 'waiting' and get a new Board
    - Return the new SearRecord 
    - go back to 4 (new SearRecord as input)
-}
survived ::  SearchRecord -> PathRecords -> Board -> Board
survived newLuckyer newWaiting board@Board{..}=board {waiting=newWaiting, lucky =newLuckyer:lucky}




{-Step2 defeatDetection Auxiliaries-}

-- | replace defeatFilter
-- checkLuckySet :: Applicative f => b -> [a1] -> f ([a2], b, [a3])
checkLuckySet ::
    ( Has D.Language env  
    , UseRuleOnly env 
    -- , Has D.RdPrefMap env 
    -- , Has D.KnwlPrefMap env 
    , MonadReader env m
    , MonadIO m 
    , OrderingContext env 
    ) => D.Language -> SearchRecords -> m (PathRecords, D.Language, SearchRecords)
checkLuckySet seen [] = pure ([],seen,[])
checkLuckySet seen (r:rs) = do 
    (re, newSeen) <- checkLucker seen r 
    (newwait, ss , nlucy) <- checkLuckySet newSeen rs 
    case re of 
        Right sr -> pure (newwait,ss, sr : nlucy)
        Left pr -> pure (pr : newwait, ss, nlucy)

-- | replace the old testLucker
checkLucker::
        ( MonadReader env m
        , MonadIO m 
        , Has D.Language env 
        , UseRuleOnly env 
        , OrderingContext env 
        ) => D.Language -> SearchRecord -> m (Either PathRecord SearchRecord, D.Language)
checkLucker seen sr@(p,_) = do 
    conflicts <- scanAttacker seen p 
    defeaters <- concat <$> mapM (checkConflict p) conflicts 
    if null defeaters 
        then pure (Right sr, seen)
        else 
            do 
            let 
                newSeen = M.rmdups $ seen ++ defeaters      
            newPathRecord <- createPathRecord p defeaters   
            pure (Left newPathRecord, newSeen) 

-- | 
-- 
-- | TODO: 
-- 1. 'conflict <$> globalRules <*> localRules' is really a computation consuming part!
-- 2. Extend Has so that this can be test automatically using QuickCheck maybe ? 
scanAttacker ::
        ( MonadReader env m
        , MonadIO m 
        , Has D.Language env 
        ) => D.Language -> D.Path -> m [Conflict]
scanAttacker seen path = do 
    lang <- grab @D.Language 
    let 
        validRules = [r | r <-lang , r `notElem` seen] 
        localRules = concat path 
    pure $ filter ( /= Peace) $ conflict <$> validRules <*> localRules 

checkConflict :: 
    ( MonadReader env m 
    , MonadIO m 
    , Has D.Language env 
    , UseRuleOnly env 
    , OrderingContext env 
    ) => D.Path -> Conflict -> m D.Language 
checkConflict _ (Undercut l)= pure [l]
checkConflict _ Peace = pure [] 
checkConflict p (Rebut l) = do 
    let 
        defP = branchDef p [M.neg l]
    necPaths <- getNecPath l
    if 
        null defP 
        then pure []
        else  do 
            rs <- mapM (checkDefeat defP) necPaths 
            if or rs 
                then pure [l] 
                else pure []  


createPathRecord ::
        ( MonadReader env m
        , MonadIO m 
        , UseRuleOnly env
        )=> D.Path ->  D.Language -> m PathRecord
createPathRecord p defeaters = do 
    tmpAgu <- initAgu defeaters
    pure (p,tmpAgu)

-- | This is the part which should be further abstracted. 
branchDef :: D.Path -> D.Language -> D.Path 
branchDef mp [] = []
branchDef mp lang = 
    let 
        rules = concat mp 
        currentLevel = [ r |l <- lang, r <- rules, D.conC r == l] 
        nextLevelPros = concat $ D.body <$> currentLevel
    in currentLevel : branchDef mp nextLevelPros

getNecPath :: 
    ( MonadReader env m 
    , MonadIO m 
    , Has D.Language env 
    , UseRuleOnly env 
    ) => D.Literal -> m D.Argument 
getNecPath l = do 
    initA <- initAgu [l]
    augDefeasible initA

checkDefeat :: 
    ( MonadReader env m 
    , MonadIO m 
    , OrderingContext env
    ) => D.Path -> D.Path -> m Bool 
checkDefeat defenderPath attackPath = do 
    rdPreferenceMap <- D.getRdPrefMap <$>   grab @D.RdPrefMap
    knPreferenceMap <- D.getKnwlPrefMap <$> grab @D.KnwlPrefMap
    let prefMap = Map.union rdPreferenceMap knPreferenceMap
        (flg, dDefs) = lastLinkChecker defenderPath (D.conC <$> head defenderPath)
    if flg 
    then
        do 
        let 
            aDefs = [r | r <- concat attackPath, D.imp r == M.D]
        pure $ ord prefMap aDefs dDefs 
        
    else 
        pure flg 

selectOneLucker :: SearchRecords -> SearchRecord 
selectOneLucker rs = 
    let 
        completePath =  [r | r <- rs, reachGround (fst r) ]
        shortestPath = sortBy (flip compare `on` (length . fst)) completePath 
    in head shortestPath

{-Ord Auxiliaries-}
ord :: D.PreferenceMap -> D.Language -> D.Language -> Bool 
ord pm attackerDefs argDefs 
    | null ldrA && null ldrB = eli pm (axiA ++ ordiA) (axiB ++ ordiB)
    | null ldrA = True 
    | otherwise = eli pm ldrA ldrB 
    where 
        ldrA = rulesToDefs attackerDefs
        ldrB = rulesToDefs argDefs
        axiA = rulesToPrems attackerDefs 
        axiB = rulesToPrems argDefs
        ordiA = rulesToAxiom attackerDefs 
        ordiB = rulesToAxiom argDefs

eli :: D.PreferenceMap -> D.Language -> D.Language -> Bool 
eli pMap argA argB 
        | null argA && null argB = False 
        | null argB = False 
        | null argA  =  True 
        | otherwise = eli' pMap argA argA

eli' :: D.PreferenceMap -> D.Language -> D.Language -> Bool 
eli' pMap l1 (l:ls)= 
    let 
        rl = [sl | sl <- l1 , preferThan pMap sl l]
    in
        ((length rl == length l1) || eli' pMap l1 ls)
eli' pMap l1 [] = False

preferThan pMap l1 l2 = 
    let cMay = do 
                r1 <- Map.lookup (D.name l1) pMap 
                r2 <- Map.lookup (D.name l2) pMap 
                pure $ r1 >= r2 
    in Just True == cMay


rulesToDefs :: D.Language -> D.Language 
rulesToDefs rules = [r | r<-rules, (not . null) (D.body r)]

rulesToPrems :: D.Language -> D.Language 
rulesToPrems rules = [r | r<-rules, D.imp r == M.D , null (D.body r)]

rulesToAxiom:: D.Language -> D.Language 
rulesToAxiom rules = [r | r<-rules, D.imp r == M.S , null (D.body r)]

-- | 
-- If given path contains all last links of path to given 'props' then returns (True and corresponding rules)
lastLinkChecker :: D.Path -> D.Language -> (Bool, D.Language) 
lastLinkChecker p targets = 
    let 
        rules = concat p 
    in case lastLinkScan rules targets of 
        Nothing -> (False , [])
        Just defs -> (True, defs)

lastLinkScan :: D.Language -> D.Language -> Maybe D.Language 
lastLinkScan _ [] = Just [] 
lastLinkScan rules targets = do 
    let 
        rs = [r | r <- rules, D.conC r `elem` targets]
    if length rs /= length targets 
            then Nothing 
            else 
                let 
                    strictLine = [ r | r <- rules, D.conC r `elem` targets, D.imp r == M.S, (not . null) (D.body r)]
                    defeasible = [ r | r <- rules, D.conC r `elem` targets, D.imp r == M.D, (not . null) (D.body r)]
                    premise = [ r|  r <- rules, D.conC r `elem` targets, null (D.body r)]
                in do 
                    rlist <- lastLinkScan rules (concat (D.body <$> strictLine))
                    pure $ rlist ++ defeasible ++ premise 
            

selectRebutters :: D.Language -> D.Language -> D.Path -> D.Language
selectRebutters lang seen p = 
    let 
        defeasiblePropositions = [D.conC r | r <- concat p, D.imp r == M.D ]
        targets = M.neg <$> defeasiblePropositions
    in [c | c <- lang , c `elem` targets , c `notElem` seen]

selectUndercutters :: D.Language -> D.Language -> D.Path -> D.Language 
selectUndercutters lang seen p = 
    let 
        defeasibleRules = [r | r <- concat p, D.imp r == M.D && (not . null) (D.body r)]
        targets = M.neg <$> defeasibleRules
    in [l | l <- lang , l `elem` targets, l `notElem` seen]


{-construction auxilaries-}
initAgu:: 
        ( MonadReader env m
        , MonadIO m 
        , UseRuleOnly env
        )=> D.Language -> m D.Argument 
initAgu ls = do
    subLevel <- mapM concludeBy ls 
    pure [[p] | p <- foldr createParallel [[]] subLevel]

createParallel :: [a] -> [[a]] -> [[a]] 
createParallel paths ls = do 
        pa <- paths
        a <- ls 
        pure $  pa:a  

aguFixpoint :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => D.Argument  -> m D.Argument 
aguFixpoint argument = do 
    extendedAgu <- agu argument 
    if extendedAgu == argument 
        then pure argument 
        else aguFixpoint extendedAgu 

augDefeasible :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => D.Argument  -> m D.Argument 
augDefeasible = aguFixpoint 

agu :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => D.Argument  -> m D.Argument 
agu argument = do 
        arguments <- mapM pathExtend argument 
        pure $ concat arguments 

pathExtend :: 
    ( MonadReader env m
    , UseRuleOnly env 
    , MonadIO m 
    ) => D.Path -> m D.Argument 
pathExtend path = do 
    let 
        rules = last path 
        bodies = concat $ D.body <$> rules 
    if 
        null bodies 
        then pure [path]
    else 
        do 
    supportRules <- mapM concludeBy bodies 
    let 
        parallelPathSection = foldr createParallel [[]] supportRules
        newArgument = do 
                pathSection <- parallelPathSection
                pure $ path ++ [pathSection] 
    pure newArgument
        

concludeBy :: 
        ( MonadReader env m
        , MonadIO m 
        , UseRuleOnly env
        ) => D.Literal -> m D.Language 
concludeBy l = do 
    dRules <- grab @D.DefeasibleRules
    sRules <- grab @D.StrictRules
    let 
        globalRules = D.getDefeasibleRules dRules ++ D.getStrictRules sRules
    pure [r | r<- globalRules, D.conC r == l ]

checkLuckyComplete :: SearchRecords -> SearchRecords
checkLuckyComplete rs = [r | r <- rs, reachGround (fst r) ]

reachGround :: D.Path -> Bool 
reachGround path = 
    let 
        i = last path 
    in case concat (D.body <$> i) of 
        [] -> True 
        _ -> False 