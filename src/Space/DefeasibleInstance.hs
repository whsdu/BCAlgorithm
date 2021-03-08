{-# LANGUAGE MultiParamTypeClasses #-}
module Space.DefeasibleInstance where 

import qualified Space.Meta as M 
import qualified Space.DefeasibleFrame as DF 
import qualified Space.Defeasible as D 
import qualified Utility.Defeasible as UD 

import Data.Maybe (fromMaybe)


data Conflict = Rebut D.Literal | Undercut D.Literal | Peace deriving (Eq, Show)

instance DF.Attack D.Literal Conflict where 
    conflict proposition rule = 
        case D.imp rule of 
            M.S -> Peace 
            M.N -> Peace 
            M.D -> 
                fromMaybe 
                    (fromMaybe Peace $ rebuts proposition rule) 
                    (undercuts proposition rule)


rebuts :: D.Literal -> D.Literal -> Maybe Conflict 
rebuts proposition rule = 
    if M.neg proposition == D.conC rule 
        then Just $ Rebut proposition 
        else Nothing 

undercuts :: D.Literal -> D.Literal -> Maybe Conflict 
undercuts ruleAttacker rule = 
    if M.neg (D.conC ruleAttacker)== rule 
        then Just $ Undercut (D.conC ruleAttacker)
        else Nothing 

