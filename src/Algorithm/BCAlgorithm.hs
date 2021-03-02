{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE TupleSections #-}

module Algorithm.BCAlgorithm where 

import qualified Data.HashMap.Strict as Map 
import qualified Space.Defeasible as D
import EnvDef  



type Base = D.Argument 

data ArgumentStatus = Warranted | Unwarranted 

data Defeater = SW D.Path | Node [(D.Path,Defeater)]

type SearchRecord = (D.Path,Defeater)
type SearchRecords = [SearchRecord]

type PathRecord = (D.Path,D.Argument) 
type PathRecords = [PathRecord]

data Board = Board {lucky :: SearchRecords , waiting :: PathRecords, futile :: SearchRecords, seen :: D.Language}

query :: D.Argument -> Defeater 
query incArgu = 
    let 
        initBoard = initialBoard incArgu 
    in chainConstruction initBoard 

chainConstruction :: Board -> Defeater 
chainConstruction step1Board = 
    case defeatCheck step1Board of                          -- step 2 : check defeat  : update 'lucky' & 'waiting'
        Right p -> warranted p                                  -- step 5: return warranted defeater  
        Left step2Board -> 
            case pathSelection step2Board of                -- step 3: path selection  : update 'lucky' , set 'base'
                Just (base,step3Board) -> 
                    let 
                        luckyExtend= augConstruction base   -- step 4: new lucky set 
                        step3Lucky = lucky step3Board
                        step4Board = step3Board{lucky=step3Lucky++luckyExtend}
                    in chainConstruction step4Board
                Nothing -> 
                    case luckyEmpty step2Board of           -- step 6: update 'waiting' for sure , update 'lucky' (step 9) or 'futile' (step 7)
                        Right unw -> unw                        -- step 8
                        Left step6Board -> chainConstruction step6Board

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
defeatCheck :: Board -> Either Board SearchRecord
defeatCheck board@Board{..}= 
    let 
        (tmpWaiting, newSeen, newLucky) = checkDefeater lucky seen          -- note 1
    in 
        if checkLuckyComplete newLucky 
            then 
                Right $ selectOneLucker newLucky                            -- note 2
            else 
                Left $ board{lucky = newLucky,waiting = waiting ++ tmpWaiting, seen=newSeen}

checkDefeater :: SearchRecords -> D.Language  -> (PathRecords,D.Language, SearchRecords)
checkDefeater srs seen = undefined 

selectOneLucker :: SearchRecords -> SearchRecord 
selectOneLucker = undefined 

checkLuckyComplete :: SearchRecords -> Bool 
checkLuckyComplete = undefined 
    




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
selectionOne = undefined 


{- 4
- Given SearchRecord selected from step 3, continue construction. 
    - More path may be discovered, thus append existing Defeater to all new paths. 
    - These new paths are New SearchRecords.
- Get a new 'lucky' = New SearchRecords + old lucky (SearchRecords). 

- get New Board with new 'lucky' from Board in step 3 
- go to step 2 (input new Board)
-}
augConstruction :: SearchRecord -> SearchRecords
augConstruction (p,defeater) = 
    let 
        incArgument = pathExtend p 
    in (,defeater) <$> incArgument


pathExtend :: D.Path -> D.Argument 
pathExtend = undefined 
-- pathExtend :: 
    -- ( MonadReader env m
    -- , UseRuleOnly env 
    -- , MonadIO m 
    -- ) => L.Path -> m L.EquifinalPaths 
-- pathExtend path = do 
    -- let 
        -- rules = last path 
        -- bodies = concat $ L.body <$> rules 
    -- if 
        -- null bodies 
        -- then pure [path]
    -- else 
        -- do 
        -- supportRules <- mapM concludeBy bodies 
        -- let 
            -- parallelPathSection = foldr parallel [[]] supportRules
            -- newArgument = do 
                    -- pathSection <- parallelPathSection
                    -- pure $ path ++ [pathSection] 
        -- pure newArgument
    -- where 
        -- parallel :: [a] -> [[a]] -> [[a]] 
        -- parallel paths ls = do 
                -- pa <- paths
                -- a <- ls 
                -- pure $  pa:a 


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
luckyEmpty :: Board -> Either Board Defeater 
luckyEmpty board@Board{..} = case waiting of 
    [] -> Right $ unwarranted futile 
    _ -> let 
            (p,incArgument) = selectionTwo waiting 
            result = query incArgument 
         in case tellQuery result of 
                Warranted -> 
                    let 
                        newBoard = waitingToFutile (p,result) (p,incArgument) board
                    in  luckyEmpty newBoard
                Unwarranted -> 
                    let 
                        newBoard = survived (p,result) (p,incArgument) board
                    in  Right $ chainConstruction newBoard 


selectionTwo :: PathRecords -> PathRecord
selectionTwo = undefined 

tellQuery :: Defeater -> ArgumentStatus 
tellQuery = undefined 

{- 7
In step 6, given the defeater is Warranted. We have a new SearchRecord  
    - Remove old PathRecord from 'waiting'
    - Move new SearchRecord  to 'futile'
        - go back to 6 
-}
-- TODO: futile should be [(Path, Defeater)]
waitingToFutile :: SearchRecord -> PathRecord -> Board -> Board
waitingToFutile = undefined 

{- 8 
Given 'lucky' and 'waiting' are all empty.  
Then all defeater are warranted. In this case
    - convert futile :: [(Path,Defeater)] to Node [(Path,Defeater)]
-}
unwarranted :: SearchRecords -> Defeater 
unwarranted = undefined 

{- 9 
Given that the PathRecord related defeater is unwarranted. We have a new SearchReacord
    - Remove old PathRecord from 'waiting' and get a new Board
    - Return the new SearRecord 
    - go back to 4 (new SearRecord as input)
-}
survived ::  SearchRecord -> PathRecord -> Board -> Board
survived = undefined 