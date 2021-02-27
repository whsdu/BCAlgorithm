{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}

module Literal where

import           Data.List (intercalate)
import           Control.Monad (guard)


data Name = Name 
            { alias :: String
            , notNeg :: Bool 
            }
data Imp = S | D

instance Show Imp where
    show S = " -> "
    show D = " ~> "

class ShowName a where
    name :: a -> String 

class GetName a where 
    getName :: a -> Name 

type Empty = []

data Literal where
    Meta :: Name -> Literal
    Neg :: Name -> Literal -> Literal
    Rule :: Name -> Imp -> [Literal] -> Literal -> Literal


instance ShowName Literal where
    name (Meta n)        = if notNeg n then alias n else "neg-" ++ alias n
    name (Rule n _ _ _ ) = if notNeg n then alias n else "neg-" ++ alias n
    name (Neg n _ )      = if notNeg n then alias n else "neg-" ++ alias n

instance Show Literal where
    show l@(Meta n) = name l
    show l@(Neg n _) = name l
    show l@(Rule n imp bs h) =name l ++ ": " ++ body ++ im ++ head
        where
            body = intercalate " " $ name <$> bs
            head = name h
            im = show imp

instance GetName Literal where 
    getName (Meta n)        = n
    getName (Rule n _ _ _ ) = n
    getName (Neg n _ )      = n

head :: Literal -> Maybe Literal
head (Rule _ _ _ h) = Just h
head _              = Nothing

body :: Literal -> Maybe [Literal]
body (Rule _ _ b _) = Just b
body _              = Nothing



imp :: Literal -> Maybe Imp
imp (Rule _ i _ _ ) = Just i
imp _               = Nothing

applicable :: Literal -> [Literal] -> Bool
applicable (Rule _ _ r _ ) ls =
    let
        literalNames = name <$> ls
        ruleName = name <$> r
    in nameSubset ruleName literalNames
applicable _ _ = False

-- | two auxiliary functions 
neg :: Literal -> Literal
neg l= 
    let 
        newAlias = alias $ getName l 
        newNeg = not . notNeg $ getName l 
    in Neg (Name newAlias newNeg) l 

nameSubset :: [String] -> [String] -> Bool 
nameSubset aSet bSet = and [a `elem` bSet | a <- aSet]

-- | Example in Efficient COnstruction of Structured Argumentation Systems
-- all literals {a,b,c,t,m,r1,r2...} and corresponding negations
a,b,c,t,m :: Literal
a = Meta $ Name "a" True
b = Meta $ Name "b" True
c = Meta $ Name "c" True
t = Meta $ Name "t" True
m = Meta $ Name "m" True

r1,r2,r3,r4,r5,r6,r7 :: Literal
r1 = Rule (Name "r1" True) S [] c           -- fact
r2 = Rule (Name "r2" True) S [] t           -- fact    
r3 = Rule (Name "r3" True) S [] m           -- fact
r4 = Rule (Name "r4" True) S [] $ neg r7    -- fact 
r5 = Rule (Name "r5" True) D [] a
r6 = Rule (Name "r6" True) D [a] $ neg b
r7 = Rule (Name "r7" True) D [c,t] b

-- | ruleS and ruleD are actually Konwledge Base
-- | and all elements in KB are Basic Arguments
-- | Language contains all name of rules(KB elements) and node of the graph
-- | replace rule(KB element) with new Argument names (including nodes that support).

-- TODOs: a graph to show the operation above.

ruleS :: [Literal]
ruleS = [r1,r2,r3,r4]

ruleD :: [Literal]
ruleD = [r5,r6,r7]

language ::[Literal]
language = l
    where
        l = [a,b,c,t,m,r1,r2,r3,r4,r5,r6,r7]
        -- ngl = neg <$> l

data Argument = Argument
    { impA  :: Imp
    , concA :: Literal
    , bodyA :: [Argument]
    , nameA :: String
    }

instance ShowName Argument where
    name (Argument _ _ _ n) = n

type ArgumentLayer = [Argument]

instance Show Argument where
    show a@(Argument imp conc bodys n) = name a ++ " :" ++ nameString ++ impString ++ concAstring
        where
            nameString = intercalate " " $ name <$> bodys  
            impString = show imp
            concAstring = name conc

instance Eq Argument where 
    (==) a1 a2 = nameA a1 == nameA a2

subArgument ::  Argument -> [Argument]
subArgument a@(Argument _ _ [] _ ) = [a]
subArgument a@(Argument _ _ as _ ) = 
    let 
        h = [a]
        t = concat $ subArgument <$> as 
    in h ++ t

topRuleArgument :: Argument -> Literal 
topRuleArgument = undefined 

-- | TODOs: 
-- Currently, all facts must be defined before getBasicArgument
-- Need to start building from ruleS and ruleD (knowledge base)
getBasicArgument :: [Literal] -> [String] -> ArgumentLayer 
getBasicArgument ls aNames= knowledge'2'argument ls aNames []
    where 
        knowledge'2'argument :: [Literal] -> [String] -> ArgumentLayer -> ArgumentLayer
        knowledge'2'argument [] _ al = al 
        knowledge'2'argument (r:rs) names@(n:ns) al = 
            case r of 
              (Rule _ imp [] l) -> 
                  let newArgument = Argument 
                                      { impA = imp
                                      , concA = l 
                                      , bodyA = []
                                      , nameA = n
                                      }
                  in knowledge'2'argument rs ns (newArgument:al)
              (Rule _ imp ls l) -> 
                  let 
                      ruleBodyNames = name <$> ls 
                      argumentLayerNames = do 
                          rname <- ruleBodyNames 
                          a <- al 
                          guard $ rname == name  (concA  a)
                          return  a 
                  in    
                    if length ruleBodyNames == length argumentLayerNames 
                          then 
                              let newArgument = Argument 
                                                  { impA = imp
                                                  , concA = l 
                                                  , bodyA = argumentLayerNames
                                                  , nameA = n 
                                                  }
                                in knowledge'2'argument rs ns (newArgument:al)
                   else knowledge'2'argument rs names al 
              _ -> knowledge'2'argument rs names al 


-- | Examples: 
namePoolSize :: Integer
namePoolSize = 100
argumentNamePool :: [String]
argumentNamePool = 
    let 
        ind = show <$> [1..namePoolSize]
        h = repeat "A"
    in zipWith (++) h ind 

basicArgument :: ArgumentLayer
basicArgument = getBasicArgument (ruleS++ruleD) argumentNamePool

-- | take argument from ArgumentLayer with a given argument type
-- | TODOs: No safty check here, Argument layer may not contain argument with corresopnding name
takeArgument :: String -> ArgumentLayer -> Argument 
takeArgument n as = 
    let fl = [a |a <- as, nameA a == n]
    in Prelude.head fl 

data Prefer =  
    Prefer 
    { lower :: Argument
    , upper :: Argument
    } deriving (Show,Eq)

testDEF :: [Prefer]
testDEF = 
    let 
        t1 = Prefer 
            { lower = takeArgument "A6" basicArgument
            , upper = takeArgument "A7" basicArgument
            }
        t2 = Prefer
            { lower = takeArgument "A7" basicArgument
            , upper = takeArgument "A4" basicArgument
            }
    in [t1,t2]

data DefeatType = Rebutting | Undercutting | NotDefeat

newtype Defeat = Defeat {getDefeatPair :: (Argument, Argument)}


checkDefeatType :: Argument -> Argument -> DefeatType 
checkDefeatType attackA defendA = undefined

defeat :: [Prefer] -> Argument -> Argument -> DefeatType
defeat preferList attackA defendA = 
    let 
        verifyPair = Prefer {upper = attackA, lower=defendA}
        hasPrefer = verifyPair `elem` preferList
    in 
        if hasPrefer 
            then checkDefeatType attackA defendA 
            else NotDefeat

-- | Prcedure :
-- 1. Data set contains rules and ruleD
-- 2. set up preferences mannually ( or maybe it is contained in the dataset as well).
-- 2. Buid basic argument from ruleS and ruleD

-- | TODOs:
-- 1. Define type level expression of different layer (Fact, common Proposition, Rule, Literal)
--    These layer could be defined in different module
--    Examples and test in Paper should be in different modules
-- 2. Define Type Level , Show , Eq constrain
-- 3. rewite all above functions
-- 4. Check other logic programming examples to get inspiration

