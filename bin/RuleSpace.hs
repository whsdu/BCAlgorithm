module RuleSpace
    ( Rule (..)
    , RuleCore(..)
    )
    where


import qualified MetaDefinition as M

data Rule = 
    Rule
    { ruleName :: M.Name
    , ruleCore :: RuleCore
    }

data RuleCore  = 
    RuleCore 
    { ruleBody :: [L]
    , ruleImp :: M.Imp
    , ruleHead :: L
    } 

data L where 
    R :: M.Name -> RuleCore -> L
