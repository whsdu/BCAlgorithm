module Utility.DomainModel where 

import qualified Data.HashMap.Strict as Map 
import qualified Space.Defeasible as D
import EnvDef  


-- | Return result from two boundary conditions
-- Succ : Warranted Argument 
-- Fail : Unwarranted Argument
-- TODO: warranted could be temporary how to record this . 

-- | TODO:
-- 一个 path 上每个 target 都会有一个 defeater， 所以一个
-- 
-- 一个path 可能有N 个defeater， 可能头几个 Defeater 不是 warranted 的， 但是后面又出现了warranted 的，
-- A：此时如果最终 query argument 不是 warranted ， 需要提供 他的defeater的证据时， 这部分变节的怎么算 ？ （好像是个新情况哦）
-- 这东西应该是一个 带 succ 和 上面提到的那个信息 的 一个 tree

-- data State = Succ | Fail 

-- | Wrap a path to be an argument 
type Base = D.Argument 
-- | 
-- Argument = Lucky , a set of incomplete paths
-- Map D.Path D.Argument = Waiting : Defeander  , 如果 上面 A 情况出现， 意味着 Defender 多了一个path， 意味着 下面一层多了一个defender 而已.
-- data Buffer  = Buff {lucky :: Con D.Argument, waiting :: Map D.Path D.Argument}


-- data Defeater = Succ D.Argument | Fail D.Argument | Yet D.Argument 

data Defeater = SW D.Path | Node [(D.Path,Defeater)]

type SearchRecord = (D.Path,Defeater)
type SearchRecords = [SearchRecord]

type PathRecord = (D.Path,D.Argument) 
type PathRecords = [PathRecord]
-- |TODOs (lucky , waiting, futile) should be of different types 
-- lucky and futile should be (D.Path, Defeater)
-- waiting be (D.Path, Argument)
data Board = Board {lucky :: SearchRecords , waiting :: PathRecords, futile :: SearchRecords, seen :: D.Language}


{- 1
-- Given a set of rules (this set of rules concludes to a set of proposition , old equifinal path section)
        - Initialize these in-complete paths SearchRecord == 'lucky' (each SearchRecord has a Defeater as 'Node[]')
        - Initialize empty PathRecords      [] == 'waiting' 
        - Initialize empty SearchRecords    [] == 'futile'  
-}
-- go to 2 
initial :: D.Language -> Board 
initial = undefined 


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
-}
-- | TODO: what if many pathes (in lucky) are all complete in this round of construction.
defeatCheck :: Board -> Either Board SearchRecord
defeatCheck = undefined 

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
pathSelection = undefined 

{- 4
- Given SearchRecord selected from step 3, continue construction. 
    - More path may be discovered, thus append existing Defeater to all new paths. 
    - These new paths are New SearchRecords.
- Get a new 'lucky' = New SearchRecords + old lucky (SearchRecords). 
    - get New Board with new 'lucky'
- go to step 2 (input new Board)
-}
augConstruction :: SearchRecord -> Board 
augConstruction = undefined 


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
warranted = undefined 

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
luckyEmpty = undefined 

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
unwarranted :: Board -> Defeater 
unwarranted = undefined 

{- 9 
Given that the PathRecord related defeater is unwarranted. We have a new SearchReacord
    - Remove old PathRecord from 'waiting' and get a new Board
    - Return the new SearRecord 
    - go back to 4 (new SearRecord as input)
-}
survived ::  PathRecord -> Board -> (Board,SearchRecord)
survived = undefined 