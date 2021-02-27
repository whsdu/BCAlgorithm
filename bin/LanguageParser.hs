module Parser.LanguageParser where 

import qualified Space.Language as L ( Literal(Rule), name) 
import qualified Space.Argumentation as A ( ArgumentationSpace, Argumentation(..))
import Control.Monad (guard)



-- | check if same literal in data source is valid 
-- 1. same atom literal has different Imp  (-> and ~>)
-- 2. other properties need to satisfied.
parsLanguageCheck :: [L.Literal] -> [L.Literal]
parsLanguageCheck = undefined 

parsBasicArgument :: [L.Literal] -> [String] -> A.ArgumentationSpace
parsBasicArgument ls aNames= knowledge'2'argument ls aNames []
    where 
        knowledge'2'argument :: [L.Literal] -> [String] -> A.ArgumentationSpace -> A.ArgumentationSpace
        knowledge'2'argument [] _ al = al 
        knowledge'2'argument (r:rs) names@(n:ns) al = 
            case r of 
              (L.Rule _ [] imp c) -> 
                  let newArgument = A.Argumentation 
                                      { A.imp = imp
                                      , A.conC = c 
                                      , A.body = []
                                      , A.name = n
                                      }
                  in knowledge'2'argument rs ns (newArgument:al)
              (L.Rule _ bs imp c) -> 
                  let 
                      ruleLiteralBodyNames = L.name <$> bs 
                      ruleLiteralBodyArgs= do 
                          rname <- ruleLiteralBodyNames 
                          a <- al 
                          guard $ rname == L.name (A.conC a)
                          return  a 
                  in    
                    if length ruleLiteralBodyNames <= length ruleLiteralBodyArgs
                          then 
                              let newArgument = A.Argumentation
                                                  { A.imp =  imp
                                                  , A.conC = c
                                                  , A.body = ruleLiteralBodyArgs
                                                  , A.name = n 
                                                  }
                                in knowledge'2'argument rs ns (newArgument:al)
                   else knowledge'2'argument rs names al 
              _ -> knowledge'2'argument rs names al 


-- | How the preference was computed

-- | types being used for testing and demo examples

