module Utility.DomainModel where 

import qualified Data.HashMap.Strict as Map 
import qualified Space.Defeasible as D
import EnvDef  


-- | Return result from two boundary conditions
-- Succ : Warranted Argument 
-- Fail : Not-warranted Argument
-- TODO: warranted could be temporary how to record this . 

-- | TODO:
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


data Defeater = Succ D.Argument | Fail D.Argument | Yet D.Argument 
type PathRecord = (D.Path,[Defeater])
type PathRecords = [PathRecord]
data Board = Board {lucky :: PathRecords, waiting :: PathRecords, futile :: PathRecords}


{- 1
-- Given a set of rules (this set of rules concludes to a set of proposition , old equifinal path section)
        - Initialize these in-complete paths as PathRecords == get 'lucky' (each PathRecord with empty [Defeater])
        - Initialize empty PathRecords == get 'waiting'
        - Initialize empty PathRecords == get 'futile'
-}
-- go to 2 
initial :: D.Language -> Board 
initial = undefined 


{- 2
- Given Board from step 1
    - Check 'lucky': If any Defeater found for a path: 
        - Initialize all defeaters as Yet, append to related Path , move this PathRecord to 'waiting'
    - Check if any PathRecord in 'lucky' contains complete Path. 
        - Yes: go to 5 
            - Exit : Path the complete Path Record to 5 and get return result 
        - No : go to 3 
            - Continue construction
-}
defeatCheck :: Board -> Either Board PathRecord 
defeatCheck = undefined 

{- 3
- Given Board from step 2
    - If 'lucky' contains more than one PathRecord , select One (Path selection strategy)
        - Set this one as 'Base' :: PathRecord
        - Move this PathRecord out of lucky , get Old PathRecord
        - go to 4 == Just PathRecord
    - If 'lucky' is empty 
        - go to 6 == Nothing 
-}
luckSelection :: Board -> Maybe PathRecord 
luckSelection = undefined 

{- 4
- Given PathRecord selected from step 3, continue construction. 
    - More path may be discovered, thus append existing Defeater to all new paths. 
    - Because these are defeaters with lower composition order. 
    - These new paths are New PathRecords.
- Get a new Board  == New PathRecords + PathRecord. 
    - put all NewPathRecord to 'lucky'
- go to step 2
-}
augConstruction :: PathRecord -> Board 
augConstruction = undefined 


{- 5
- Given a complete path record :: PathRecord. 
    - wrap the path as an argument , 
    - append return result from failed defeaters
-}
warranted :: PathRecord -> Either State 
warranted = undefined 


{--}

{- 6 
Known 'lucky' is empty  from step 3
- Select a PathRecord from 'waiting' according to Step 10
- Check if the defeater is Warranted. 
    - If 'waiting' is empty 
        - go to  step 8 
    - if the defeater is Warranted . 
        - go to 7 
    - if the Defeaters is not-Warranted. 
        - go to 9 
-}
luckyEmpty :: Board -> Either State 
luckyEmpty = undefined 

{- 7
In step 6, given the defeater is Warranted. 
    - Move the PathRecord to 'futile'
        - go back to 7 
-}
waitingReduction :: Board -> Either State 
waitingReduction = undefined 

{- 8 
Given 'lucky' and 'waiting' are all empty. 
Exit condition :
-}

