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


{- 1
-- Given a set of rules (this set of rules concludes to a set of proposition , old equifinal path section)
        - Initialize these in-complete paths SearchRecord == 'lucky' (each SearchRecord has a Defeater as 'Node[]')
        - Initialize empty PathRecords      [] == 'waiting' 
        - Initialize empty SearchRecords    [] == 'futile'  
Notes: 
    1. incPath is of type Path = [Rules,...,Rules] 
    2. Rules in [Rules,...,Rules] has same set of heads. 
    3. Each Rules in [Rules,...,Rules] is a head of an incomplete-path.

-}
-- go to 2 
initial :: D.Argument -> Board 
initial incArgu = 
    let 
        luckySet = (\p -> (p,Node[])) <$> incArgu 
    in Board luckySet [] [] []



{- 2
- Given Board from step 1
    - Check 'lucky': If any unseen in-defeater found for each path: 
        - include all newly defeated proposition to seen
        - Attaches the in-defeater to related Path , move this PathRecord to 'waiting'
        - If path has old related defeater(much be failed defeater), then remove this old defeater and attach this new in-defeater. 
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
                Left $ board{lucky = newLucky,waiting = waiting ++ tmpWaiting}

checkDefeater :: SearchRecords -> D.Language  -> (PathRecords,D.Language, SearchRecords)
checkDefeater = undefined 

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
pathSelection :: Board -> Maybe SearchRecord 
pathSelection Board{..} = case lucky of 
    [] -> Nothing 
    _ -> Just $ selectionOne lucky

selectionOne :: SearchRecords -> SearchRecord 
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
luckyEmpty :: Board -> Defeater 
luckyEmpty Board{..} = case waiting of 
    [] -> unwarranted futile 
    _ -> let 
            (p,incArgument) = selectionTwo waiting 
            result = query inArgument 
            case tellQuery result of 
                Warranted -> waitingReduction 
                Unwarranted -> survived 

-- | TODO: assemble these functions together  as query 
query :: D.Argument -> Defeater 
query = undefined 

tellQuery :: Defeater -> ArgumentStatus 
tellQuery = undefined 

{- 7
In step 6, given the defeater is Warranted. We have a new SearchRecord  
    - Remove old PathRecord from 'waiting'
    - Move new SearchRecord  to 'futile'
        - go back to 6 
-}
-- TODO: futile should be [(Path, Defeater)]
waitingReduction :: SearchRecord -> PathRecord -> Board -> Board 
waitingReduction = undefined 

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
survived ::  PathRecord -> Board -> (Board,SearchRecord)
survived = undefined 