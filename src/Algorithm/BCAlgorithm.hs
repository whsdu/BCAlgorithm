{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE TupleSections #-}

module Algorithm.BCAlgorithm where 

import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map 
import qualified Space.Defeasible as D
import qualified Utility.Defeasible as MD
import qualified Space.Meta as M 
import EnvDef  
import Data.List (sortBy)
import Data.Function (on)



type Base = D.Argument 

data ArgumentStatus = Warranted | Unwarranted 

data Defeater = SW D.Path | Node [(D.Path,Defeater)]

type SearchRecord = (D.Path,Defeater)
type SearchRecords = [SearchRecord]

type PathRecord = (D.Path,D.Argument) 
type PathRecords = [PathRecord]

data Board = Board {lucky :: SearchRecords , waiting :: PathRecords, futile :: SearchRecords, seen :: D.Language}

query ::D.PreferenceMap -> D.Language -> D.Argument -> Defeater 
query pm lang incArgu = 
    let 
        initBoard = initialBoard incArgu 
    in chainConstruction pm lang initBoard 

chainConstruction ::D.PreferenceMap -> D.Language -> Board -> Defeater 
chainConstruction pm lang step1Board = 
    case defeatCheck pm lang step1Board of                          -- step 2 : check defeat  : update 'lucky' & 'waiting'
        Right p -> warranted p                                  -- step 5: return warranted defeater  
        Left step2Board -> 
            case pathSelection step2Board of                -- step 3: path selection  : update 'lucky' , set 'base'
                Just (base,step3Board) -> 
                    let 
                        luckyExtend= augConstruction lang base   -- step 4: new lucky set 
                        step3Lucky = lucky step3Board
                        step4Board = step3Board{lucky=step3Lucky++luckyExtend}
                    in chainConstruction pm lang step4Board
                Nothing -> 
                    case luckyEmpty pm lang step2Board of           -- step 6: update 'waiting' for sure , update 'lucky' (step 9) or 'futile' (step 7)
                        Right unw -> unw                        -- step 8
                        Left step6Board -> chainConstruction pm lang step6Board

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
defeatCheck :: D.PreferenceMap ->  D.Language -> Board -> Either Board SearchRecord
defeatCheck pm lang board@Board{..}= 
    let 
        (newWaiting, newSeen, newLucky) = defeaterFilter pm lang seen lucky    -- note 1
    in 
        if checkLuckyComplete newLucky 
            then 
                Right $ selectOneLucker newLucky                            -- note 2
            else 
                Left $ board{lucky = newLucky,waiting = waiting ++ newWaiting, seen=newSeen}

defeaterFilter :: D.PreferenceMap -> D.Language ->  D.Language -> SearchRecords-> (PathRecords, D.Language, SearchRecords)
defeaterFilter pm _    seen [] = ([],seen,[])
defeaterFilter pm lang seen (r:rs) = 
    let 
        (re, newSeen)  = testLucker pm lang seen r 
        (newwait, ss , nlucy) = defeaterFilter pm lang newSeen rs 
    in 
        case re of 
            Right sr -> (newwait,ss, sr : nlucy)
            Left pr -> (pr : newwait, ss, nlucy)

testLucker ::D.PreferenceMap -> D.Language -> D.Language -> SearchRecord -> (Either PathRecord SearchRecord, D.Language)
testLucker pm  lang seen sr@(p,_) = 
    let 
        rebutters = selectRebutters lang seen p 
        undercutters = selectUndercutters lang seen p 
        defeaters = selectDefeaters pm lang p rebutters undercutters
    in 
        if null defeaters 
            then (Right sr, seen)
            else 
                let 
                    newSeen = updateSeen seen defeaters 
                    newPathRecord = createPathRecord lang p defeaters
                in (Left newPathRecord, newSeen )

updateSeen :: D.Language -> D.Language -> D.Language 
updateSeen old defs = M.rmdups $ old ++ defs 

createPathRecord :: D.Language -> D.Path ->  D.Language -> PathRecord
createPathRecord lang p defeaters = (p, initIncArgument lang defeaters)

-- | TODO: Rebuters winner path need to be preserved
selectDefeaters ::D.PreferenceMap -> D.Language -> D.Path -> D.Language -> D.Language -> D.Language
selectDefeaters _  _    _     [] uns@(x:_)= uns 
selectDefeaters pm lang p   res uns = checkRebutters lang p res ++ uns
    where 
        checkRebutters :: D.Language -> D.Path -> D.Language -> D.Language 
        checkRebutters _    _ [] = []
        checkRebutters lang p (u:us) = 
            case lastLinkChecker p [M.neg u] of 
                (False,_) -> checkRebutters lang p us 
                (True, defRules) -> 
                    let 
                        attackDefs = attackerLastScan lang u 
                        ordPrem = ord pm 
                    in 
                        if any (`ordPrem` defRules) attackDefs 
                        then u : checkRebutters lang p us 
                        else checkRebutters lang p us 

-- | TODO: 构建 defeater 的 path 应该把有效的保留下来， 不同长度的也可以完美应对，因为其实每次的base都只有一个，所以无所谓的.
--  and this is acutally a great optimization point. 
attackerLastScan :: D.Language -> D.Literal -> [D.Language]
attackerLastScan lang l = 
    let 
        initArg = initIncArgument lang [l]  
    in  completeLastChecker lang initArg 

initIncArgument :: D.Language -> D.Language -> D.Argument 
initIncArgument lang props = 
    let 
        rulesSet = concludeBy lang <$> props 
    in  [[p] |p <- foldr createParallel [[]] rulesSet]
    where 
        createParallel :: [a] -> [[a]] -> [[a]] 
        createParallel paths ls = do 
                        pa <- paths
                        a <- ls 
                        pure $  pa:a         


concludeBy :: D.Language -> D.Literal -> D.Language 
concludeBy lang l = [r | r <- lang, D.conC r == l ]

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

completeLastChecker :: D.Language -> D.Argument -> [D.Language]
completeLastChecker _   []        = [] 
completeLastChecker lang argument = 
    let
        a = do 
            p <- argument 
            let c = lastLinkChecker p lang 
            guard $ fst c 
            pure $ snd c 
        b = do 
            p <- argument 
            let c = lastLinkChecker p lang
            guard $ not (fst c) 
            pure p 
    in a ++ completeLastChecker lang (agu lang b)


agu :: D.Language  -> D.Argument -> D.Argument 
agu lang argument =
    let  
        arguments = pathExtend lang <$> argument 
    in concat arguments 

pathExtend :: D.Language -> D.Path -> D.Argument 
pathExtend lang path = 
    let 
        rules = last path 
        bodies = concat $ D.body <$> rules 
    in 
        if null bodies 
            then [path]
            else 
                let  
                    supportRules = concludeBy lang <$> bodies 
                    parallelPathSection = foldr parallel [[]] supportRules
                    newArgument = do 
                            pathSection <- parallelPathSection
                            pure $ path ++ [pathSection] 
                in newArgument
    where 
        parallel :: [a] -> [[a]] -> [[a]] 
        parallel paths ls = do 
                pa <- paths
                a <- ls 
                pure $  pa:a  


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
            


selectOneLucker :: SearchRecords -> SearchRecord 
selectOneLucker rs = 
    let 
        completePath =  [r | r <- rs, reachGround (fst r) ]
        shortestPath = sortBy (flip compare `on` (length . fst)) completePath 
    in head shortestPath

checkLuckyComplete :: SearchRecords -> Bool 
checkLuckyComplete rs = null [r | r <- rs, reachGround (fst r) ]

reachGround :: D.Path -> Bool 
reachGround path = 
    let 
        i = last path 
    in case concat (D.body <$> i) of 
        [] -> True 
        _ -> False 

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

{- 3
- Given Board from step 2
    - If 'lucky' contains more than one SearchRecord, select One (Path selection strategy)
        - Set this one as 'Base' :: SearchRecord 
        - Move this SearchRecord out of lucky 
        - go to 4 == Just SearchRecord
    - If 'lucky' is empty 
        - go to 6 == Nothing 
-}
pathSelection :: Board -> Maybe (SearchRecord, Board)
pathSelection board@Board{..} = case lucky of 
    [] -> Nothing 
    _ -> Just (base, newBoard)
    where 
        (base, newlucky) = selectionOne lucky
        newBoard = board{lucky=newlucky}

selectionOne :: SearchRecords -> (SearchRecord, SearchRecords)
selectionOne srs =
    let 
        sortByLength = sortBy (flip compare `on` (length . fst)) srs 
        sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) srs 
    in (head sortByDefeasible, tail sortByDefeasible)  
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
augConstruction :: D.Language -> SearchRecord -> SearchRecords
augConstruction lang (p,defeater) = 
    let 
        tmpArg = pathExtend lang p 
    in (,defeater) <$> tmpArg 


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
luckyEmpty ::D.PreferenceMap -> D.Language -> Board -> Either Board Defeater 
luckyEmpty pm lang board@Board{..} = case waiting of 
    [] -> Right $ unwarranted futile 
    _ -> let 
            ((p,incArgument), tmpWaiting) = selectionTwo waiting 
            result = query pm lang incArgument 
         in case tellQuery result of 
                Warranted -> 
                    let 
                        newBoard = waitingToFutile (p,result) tmpWaiting board
                    in  luckyEmpty pm lang newBoard
                Unwarranted -> 
                    let 
                        newBoard = survived (p,result) tmpWaiting board
                    in  Right $ chainConstruction pm lang newBoard 


selectionTwo :: PathRecords -> (PathRecord, PathRecords)
selectionTwo prs = 
    let 
        sortByLength = sortBy (flip compare `on` (length . fst)) prs  
        sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) prs 
    in (head sortByDefeasible, tail sortByDefeasible)  
    where 
        getDef :: D.Path -> D.Language 
        getDef p = 
            let 
                rules = concat p 
            in [r | r<-rules, D.imp r == M.D]
            
tellQuery :: Defeater -> ArgumentStatus 
tellQuery = undefined 

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