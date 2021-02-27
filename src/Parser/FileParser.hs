{-# LANGUAGE OverloadedStrings #-}
module Parser.FileParser where 

import Control.Monad(guard)
import System.IO 
import Data.List.Split (splitOn)
import  qualified Data.HashMap.Strict  as Map 
import Data.Maybe(fromJust, fromMaybe)
import Data.Text(isInfixOf,pack)

import qualified Space.Language as L 
    (Literal (..), Language , LanguageMap,StrictRules(..), DefeasibleRules(..), RdPrefMap(..), 
    KnwlPrefMap(..), PreferenceMap, name,body,imp,conC)
import qualified Space.Meta as M
import Env ( Env(..))
import Utility.Ordering 

-- | Transitional data type being used to bridge the gap between file and list of Literal. 
-- File contains lines of strings that represent rules. 
-- Target is list of terms of type Literal 
-- The process is : 
-- 1. lines of string --> list of Knowledge
-- 2. List of Knowledge --> List of Literal
data Knowledge = Knowledge 
    { ruleName :: String
    , premisesName :: [String]
    , impName :: String
    , conclusionName :: String
    , preferName :: String 
    }deriving Show 

type KnowledgeSpace = [Knowledge]

{-
TODOs:
Language space in env does not contains neg of rules. 
-}
parseEnv :: FilePath -> IO Env
parseEnv filePath = do 
    k <- fileToKnowledge filePath
    (rdMap, knMap) <- fileToPrefMap filePath
    let 
        l = k2l k 
        r = chainingRule l 
    pure $ mkEnv r rdMap knMap 

stringToEnv :: String -> IO Env 
stringToEnv content = do 
    k <- stringToKnowledge content 
    (rdMap, knMap) <- stringToPrefMap content 
    let 
        l = k2l k 
        r = chainingRule l 
    pure $ mkEnv r rdMap knMap 

parseLiteralMap :: Env -> Map.HashMap M.Name L.Literal
parseLiteralMap env = 
    let 
        language = envLangSpace env 
    in Map.fromList $ zip (L.name <$> language ) language


parsePreferenceMap :: Env -> L.PreferenceMap
parsePreferenceMap env = 
    let
        rdMap = L.getRdPrefMap . envRdPrefMap $ env
        knMap = L.getKnwlPrefMap . envKnwlPrefMap $ env 
    in Map.union rdMap knMap 


parseQueryLiteral :: String -> Map.HashMap M.Name L.Literal -> L.Literal
parseQueryLiteral qName lm = fromJust $ Map.lookup qName lm 

-- | TODOs: 

-- now we have function to generate env from file 
-- 1. need function to generation env from elm interface. 
-- 2. need to function to union two env and handle possible error, such as conflict of name. 
-- 3. Absolutely no error detection and handling functions. 

-- | Auxiliary function converting string to data type knowledge 


fileToKnowledge :: FilePath -> IO KnowledgeSpace
fileToKnowledge filePath = do
    handle <- openFile filePath  ReadMode
    contents <- hGetContents handle 
    pure $ parseWord <$> removeComment (lines contents)

stringToKnowledge :: String -> IO KnowledgeSpace 
stringToKnowledge content =  pure $ parseWord <$> removeComment (lines content)

parseWord :: String -> Knowledge
parseWord w = 
    let 
        [ruleName,ruleBody] = splitOn ":" w 
        impName 
            | '-' `elem` ruleBody = "->"
            | otherwise = "=>"
        [premies,conC] = splitOn impName ruleBody 
        premisesName = splitOn "," premies 
        [conclusionName,preferName] = splitOn "," conC
    in Knowledge ruleName premisesName impName conclusionName preferName


k2l :: KnowledgeSpace -> L.LanguageMap 
k2l knowledges = constructLS knowledges Map.empty
    where 
        constructLS (k:ks) lsAcc = 
            let 
                concName = conclusionName k 
                priNames = premisesName k 
                rName = ruleName k 
                iName = impName k 
                (updateAtomAcc,primLiterals,concLiteral) = insertAtomsToLanguageSpace concName  priNames lsAcc
                updateRuleAcc = insertRuleToLanguageSpace rName iName primLiterals concLiteral updateAtomAcc
            in constructLS ks updateRuleAcc
        constructLS [] lsAcc  = lsAcc
        insertAtomsToLanguageSpace :: String -> [String] -> L.LanguageMap -> (L.LanguageMap, L.Language, L.Literal)
        insertAtomsToLanguageSpace concName priNames ls = 
            let 
                (accPrim, primLiterals) = foldr insertOneAtom (ls,[]) priNames 
                (accConc, concLiterals) = insertOneAtom concName (accPrim,[])
            in (accConc, primLiterals, head concLiterals)
            where insertOneAtom n (ll,lbs) = 
                            case Map.lookup n ll of 
                                Just b -> (ll, b:lbs)
                                Nothing -> 
                                    let newl = L.Atom n 
                                    in (Map.insert n newl ll, newl:lbs)
        insertRuleToLanguageSpace 
            :: String
            -> String 
            -> L.Language
            -> L.Literal  
            -> L.LanguageMap 
            -> L.LanguageMap
        insertRuleToLanguageSpace ruleName imp primies conclusion lspace =
            let 
                impSym = if imp == "->" then M.S else M.D 
                bodies = if head primies == L.Atom "" then [] else primies 
                ruleLiteral = L.Rule ruleName bodies impSym conclusion 
            in Map.insert ruleName ruleLiteral lspace 

chainingRule :: L.LanguageMap -> L.LanguageMap 
chainingRule knowledgeMap = 
    let 
        ruleList = [ km | km <- Map.toList knowledgeMap , (L.imp . snd) km == M.D || (L.imp . snd) km == M.S] 
        ruleMap = Map.fromList [ km | km <- Map.toList knowledgeMap , (L.imp . snd) km == M.D || (L.imp . snd) km == M.S] 
    in Map.fromList $ chaining ruleMap <$> ruleList
    where 
        chaining rm tp = 
            let
                key = fst tp 
                p = snd tp 
                name = L.name p 
                imp = L.imp p 
                body = L.body p 
                conC = L.conC p 
                newBody = searchRules rm <$> body 
                newConc = searchRules rm conC 
            in (key, L.Rule name newBody imp newConc)
        searchRules :: L.LanguageMap -> L.Literal -> L.Literal 
        searchRules rm l = 
            case l of 
                L.Rule {} -> l 
                L.Atom "" -> l 
                L.Atom _ -> 
                    let 
                        name = L.name l 
                    in 
                        if head name == '!' 
                            then 
                                let 
                                    oName = tail name 
                                    rO = Map.lookup oName rm 
                                in case rO of 
                                    Nothing -> l 
                                    Just rule -> M.neg rule 
                            else 
                                let r = Map.lookup name rm 
                                in fromMaybe l r 

-- | 
-- 1. preference map of rules and premises are separated. 
-- 2. no record of strict rules because this is not part of any preference set selection methods (weakest-link or last-link).
-- TODOs: 
-- improve the separation method of rules and premises. 
fileToPrefMap :: FilePath -> IO (L.RdPrefMap, L.KnwlPrefMap)
fileToPrefMap filePath = do 
    handle <- openFile filePath  ReadMode
    contents <- hGetContents handle 
    let 
        records = removeComment( lines contents )
        premisesLines = [r | r <- records,(":=" `isInfixOf` pack r) || (":-" `isInfixOf` pack r)]
        rulesLines = [r | r <- records, r `notElem` premisesLines && not ("->" `isInfixOf` pack r)]
        rdMap = L.RdPrefMap $ Map.fromList $ parsePre <$> rulesLines
        kwMap = L.KnwlPrefMap $ Map.fromList $ parsePre <$> premisesLines
    pure (rdMap,kwMap)
  where 
      parsePre :: String -> (M.Name,Int)
      parsePre s = 
          let 
              name = head $ splitOn ":" s 
              pre = read . last $ splitOn "," s
          in (name,pre)

stringToPrefMap :: String -> IO (L.RdPrefMap, L.KnwlPrefMap)
stringToPrefMap contents = do 
    let 
        records = removeComment( lines contents )
        premisesLines = [r | r <- records,(":=" `isInfixOf` pack r) || (":-" `isInfixOf` pack r)]
        rulesLines = [r | r <- records, r `notElem` premisesLines && not ("->" `isInfixOf` pack r)]
        rdMap = L.RdPrefMap $ Map.fromList $ parsePre <$> rulesLines
        kwMap = L.KnwlPrefMap $ Map.fromList $ parsePre <$> premisesLines
    pure (rdMap,kwMap)
  where 
      parsePre :: String -> (M.Name,Int)
      parsePre s = 
          let 
              name = head $ splitOn ":" s 
              pre = read . last $ splitOn "," s
          in (name,pre)

-- | Remove 
-- 1. # : comment line 
-- 2. ' ' : lines with empty char
-- 3. "" : lines with no char
removeComment :: [String] -> [String]
removeComment sl = [s | s<-sl ,'#' `notElem` s && ' ' `notElem` s, s /= ""] 

mkEnv :: L.LanguageMap -> L.RdPrefMap -> L.KnwlPrefMap -> Env 
mkEnv lm rdMap knMap= 
    let 
        literalList = snd <$> Map.toList lm 
        strictRule = L.StrictRules [ l | l <- literalList, L.imp l == M.S]
        defeasibleRule = L.DefeasibleRules [ l | l <- literalList, L.imp l == M.D]
        atoms = M.rmdups [ l | l <- concat (L.body <$> literalList) ++ (L.conC <$> literalList), L.imp l == M.N]
    in Env (atoms ++ L.getStrictRules strictRule ++ L.getDefeasibleRules defeasibleRule) strictRule defeasibleRule [] rdMap knMap weakestEli 