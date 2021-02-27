{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where

import           AtomSpace       (Atom (..))
import           RuleSpace       (Rule (..))

import           Data.List.Split (splitOn)
import           MetaDefinition  (Imp (..), Literal (..), Name, Negation (..),
                                  literal)


instance Negation Atom Rule where
    negation _ _ = False
    neg _ = undefined

instance Negation Rule Atom where
    negation _ _ = False
    neg _ = undefined

instance Negation Rule Rule where
    negation l1 l2
        |   literal l1 == literal l2 = False
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True
        |   otherwise = False

    neg l1
        |  head (splitOn "_" (literal l1)) == "_" =
            let nName = last (splitOn "_" (literal l1))
            in l1{ruleName = nName}
        | otherwise =
            let nName = "_" ++ literal l1
            in l1{ruleName = nName}


instance Negation Atom Atom where
    negation l1 l2
        |   literal l1 == literal l2 = False
        |   last ( splitOn "_" (literal l1) ) == literal l2 = True
        |   last ( splitOn "_" (literal l2) ) == literal l1 = True
        |   otherwise = False
    neg l1
        |  head (splitOn "_" (literal l1)) == "_" =
            let nName = last (splitOn "_" (literal l1))
            in l1{atomName = nName}
        | otherwise =
            let nName = "_" ++ literal l1
            in l1{atomName = nName}

-- | Examples before Chapter 3

a,b,c,t,m :: Atom
a = Atom "a"
b = Atom "b"
c = Atom "c"
t = Atom "t"
m = Atom "m"

-- | TODOs
-- class Negation need dependencies information otherwise there always need
-- explicit declariation about the type of function neg
r1,r2,r3,r4,r5,r6,r7 :: Rule
r1 = Rule {ruleName="r1",ruleImp=S, ruleBody=[],ruleHead= c}
r2 = Rule {ruleName="r2",ruleImp=S, ruleBody=[],ruleHead= t}
r3 = Rule {ruleName="r1",ruleImp=S, ruleBody=[],ruleHead= m}
r4 = Rule {ruleName="r1",ruleImp=S, ruleBody=[],ruleHead= neg r7 :: Rule}
r5 = Rule {ruleName="r1",ruleImp=D, ruleBody=[],ruleHead= a}
r6 = Rule {ruleName="r1",ruleImp=D, ruleBody=[],ruleHead= c}
r7 = Rule { ruleName="r1"
            , ruleImp=D
            , ruleBody= [c,t]
            , ruleHead= b}

atoms = [a,b,c,t,m]

ruleS :: [Rule]
ruleS = [r1,r2,r3,r4]

ruleD :: [Rule]
ruleD = [r5,r6,r7]


-- | TODOs:
-- Is this Literal data type really necessary ?
language :: [Literal]
language = l
    where
        rList = Literal <$> ruleS ++ ruleD
        aList = Literal <$> atoms
        l = rList ++ aList

isApplicable :: Rule -> [Name] -> Bool
isApplicable (Rule _ _ body _) literals =
    let
        bs = literal <$> body
    in and [b `elem` literals | b <- bs]

data Argument = Argument 
    { argImp :: Imp 
    , argConc :: Literal 
    , argBody :: [Argument]
    , argName :: Name 
    }

subArguments :: Argument -> [Argument]
subArguments = undefined 

-- topRule :: Argument -> 
