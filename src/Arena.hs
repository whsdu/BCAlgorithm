{-# LANGUAGE MultiParamTypeClasses #-}
module Arena where

import           qualified Data.HashMap.Strict as Map 
import           qualified Space.Argumentation as A 
import           qualified Space.Language   as L 
import           qualified Parser.FileParser as PF
import           qualified Space.Meta as M 

import           Env(Env(..))
-- | Examples before Chapter 3

a,b,c,t,m :: L.Literal
a = L.Atom "a"
b = L.Atom "b"
c = L.Atom "c"
t = L.Atom "t"
m = L.Atom "m"

r1,r2,r3,r4,r5,r6,r7 :: L.Literal
r1 = L.Rule "r1" [] M.S  c
r2 = L.Rule "r2" [] M.S  t
r3 = L.Rule "r3" [] M.S  m
r4 = L.Rule "r4" [] M.S  (M.neg r7)
r5 = L.Rule "r5" [] M.D  a
r6 = L.Rule "r6" [a] M.D  (M.neg b)
-- r6 = Rule "r6" [] D  c      -- r6:=>c
r7 = L.Rule "r7" [c,t] M.D  b

lr1 = L.Rule "lr1" [lr2] M.D lr1 
lr2 = L.Rule "lr2" [lr1] M.D lr2

atoms :: [L.Literal]
atoms = [a,b,c,t,m]

ruleS :: [L.Literal]
ruleS = [r1,r2,r3,r4]

ruleD :: [L.Literal]
ruleD = [r5,r6,r7]

rChain :: a
rChain = undefined
--------- demo in paper ---------
demoPoolSize :: Integer
demoPoolSize = 100

demoArgumentNames :: [String]
demoArgumentNames =
    let
        ind = show <$> [1..demoPoolSize]
        h = repeat "A"
    in zipWith (++) h ind

demoLanguage :: L.Language
demoLanguage = atoms ++ ruleS ++ ruleD

-- demoArguments :: A.ArgumentationSpace
-- demoArguments = PL.parsBasicArgument demoLanguage demoArgumentNames

demoPreferMap :: L.PreferenceMap
demoPreferMap = Map.fromList
    [ ("r7",2)
    , ("r6",1)
    , ("r4",3)
    ]
-- paperEnv :: Env
-- paperEnv = Env demoLanguage (L.StrictRules [r1,r2,r3,r4]) (L.DefeasibleRules [r5,r6,r7]) demoArguments demoPreferMap

-- a1,a2,a3,a4,a5,a6,a7 :: A.Argumentation
-- a7 = head demoArguments
-- a6 = head . tail $ demoArguments
-- a5 = demoArguments !! 2
-- a4 = demoArguments !! 3
-- a3 = demoArguments !! 4
-- a2 = demoArguments !! 5
-- a1 = demoArguments !! 6

-- | Benchmark test
datasetRoot :: FilePath
datasetRoot = "./Example/Chains/"
defeasibleRoot :: FilePath
defeasibleRoot = datasetRoot ++ "Defeasbile"
strictRoot :: FilePath
strictRoot= datasetRoot ++ "Strict"


-- | test File 

testPath :: String 
testPath = "./Examples/Teams/"
trickyPath = "./Examples/tricky/"
testFile :: String 
testFile = "b2.txt"
largeFile = "b7.txt"
devFile = "b3.txt"
testTricky= "tricky_rules.txt"

readTestFile :: IO PF.KnowledgeSpace
readTestFile = PF.fileToKnowledge (testPath ++ testFile)

readHardFile :: IO PF.KnowledgeSpace
readHardFile = PF.fileToKnowledge (testPath ++ largeFile)

readDevFile :: IO PF.KnowledgeSpace
readDevFile = PF.fileToKnowledge (testPath ++ devFile)

readTrickyFile :: IO PF.KnowledgeSpace
readTrickyFile = PF.fileToKnowledge (trickyPath ++ testTricky)

demoEnv :: FilePath -> IO Env
demoEnv = PF.parseEnv

-- demoLiteralMap :: Env -> Map.HashMap M.Name L.Literal
-- demoLiteralMap = PF.parseLiteralMap

-- getQueryLiteral :: String -> Map.HashMap M.Name L.Literal -> L.Literal
-- getQueryLiteral = PF.parseQueryLiteral

------------ Examples below -------------------------

a1 :: L.Literal
a1 = L.Atom "a1"

b1 = L.Atom "b1"
b2 = L.Atom "b2"
b3 = L.Atom "b3"

c1 = L.Atom "c1"
c2 = L.Atom "c2"
c3 = L.Atom "c3"
-- c4 = L.Atom "c4"
-- c5 = L.Atom "c5"

d1 = L.Atom "d1"
e1 = L.Atom "e1"

ra1 = L.Rule "ra1" [b1,b2,b3] M.S a1 

rb1 = L.Rule "rb1" [c1,c2] M.S b1 
rb2 = L.Rule "rb2" [] M.S b2 
rb3 = L.Rule "rb3" [c3] M.S b3 

rc1 = L.Rule "rc1" [] M.S c1
rc2 = L.Rule "rc2" [] M.S c2
rc3 = L.Rule "rc3" [d1] M.S c2
rc4 = L.Rule "rc4" [d1] M.S c3
rc5 = L.Rule "rc5" [e1] M.S c3
rd1 = L.Rule "rd1" [] M.S d1 
rd2 = L.Rule "rd2" [e1] M.S d1 
re1 = L.Rule "re1" [] M.S e1 

testDemoPath1 = [ra1,rb1,rb2,rb3,rc1,rc2,rc3,rc4,rc5,rd1,rd2,re1]

