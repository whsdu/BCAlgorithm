{-# LANGUAGE GADTs #-}
module Space.Language 
    ( Literal (..)
    , Language 
    , Path 
    , EquifinalPathSections
    , EquifinalPaths
    , LanguageMap
    , AnonyRule(..)
    , StrictRules (..)
    , DefeasibleRules(..)
    , PreferenceMap
    , RdPrefMap(..)
    , KnwlPrefMap(..)
    , name 
    , body
    , imp
    , conC
    , lanEqual
    )where

import Space.Meta (Name, Imp(..), Negation(..))
import qualified Data.HashMap.Strict as Map
import qualified GHC.List as GHC (head)

-- | Literal is defined recursively because body and conclusion(head) or rules could also be rule itself.
-- `Rule` is constructor of `Ordinary Premises`, `Axiom Premises`, `Strict Rules` & `Defeasible Rules`. 
-- `Atom` is constructor of conclusion other than above `Premises` or `Rules`.   
-- `n` introduced in paper maps a rule to a literal, it is not necessary here when Literal is defined recursively like this.
-- TODO: actually, Atom could also be represented by Rule, with 'Imp` being 'N', this maybe over engineered 
-- If it is possible maybe use type programming to handle this ?
data Literal where
    Atom :: Name -> Literal
    Rule :: Name -> [Literal] -> Imp -> Literal -> Literal

-- | Preference needs to be redefined 
type PreferenceMap = Map.HashMap Name Int 

newtype RdPrefMap = RdPrefMap {getRdPrefMap :: Map.HashMap Name Int}
newtype KnwlPrefMap = KnwlPrefMap { getKnwlPrefMap :: Map.HashMap Name Int}
-- | `L` language is a set of `Literal`

type Language = [Literal]
type PathSection = Language 

type Path = [PathSection]
type EquifinalPathSections = [PathSection]

type EquifinalPaths = [Path]

-- | LanguageMap is a dictionary used to query Literal with given name
type LanguageMap = Map.HashMap Name Literal 

newtype StrictRules = StrictRules {getStrictRules :: Language}
newtype DefeasibleRules = DefeasibleRules {getDefeasibleRules :: Language}


-- | name of an instantiation of type `Literal`: it plays two rules:
-- 1. To be used to guarantee the uniqueness of a `Literal`.
-- 2. To be used to defined negation with simple `!`.
name :: Literal -> Name
name (Rule n _ _ _) = n
name (Atom n)       = n

-- | Body Imp Conc
-- Get body of a rule
body  :: Literal -> [Literal]
body (Rule _ b _ _) = b
body (Atom _)       = []

-- | Body Imp Conc
-- Get Imp or a rule 
imp :: Literal -> Imp
imp (Rule _ _ i _) = i
imp (Atom _)       = N

-- | Body Imp Conc
-- Get conclusion (head) of a rule 
conC :: Literal -> Literal
conC (Rule _ _ _ h) = h
conC a@(Atom _)     = a

-- | check if two list of Literal contains same elements.
lanEqual :: Language -> Language -> Bool 
lanEqual al bl = isElemB && isElemA 
    where 
        isElemB = and [ a `elem` bl | a <- al ]
        isElemA = and [ b `elem` al | b <- bl ]

instance Show Literal where
    show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
        where
            bs = unwords $ name <$> b
            im = show i
            head = name h
    show (Atom n) = n

instance Eq Literal where
    (==) l1 l2 = name l1 == name l2

instance Ord Literal where 
    compare l1 l2 = compare (name l1) (name l2 )

-- | By default : negation a1 a2 = neg a1 == a2
instance Negation Literal where
    neg (Rule n b i h)
        |  GHC.head n == '!' =
            let nLiteral = tail n 
            in Rule nLiteral b i h
        | otherwise =
            let nLiteral = '!' : n
            in Rule nLiteral b i h
    neg (Atom n)
        |  GHC.head n  == '!' =
            let nLiteral = tail n
            in Atom nLiteral
        | otherwise =
            let nLiteral = '!' : n
            in Atom nLiteral

newtype AnonyRule = AnonyRule {unanonyRule :: Literal}

instance Eq AnonyRule where
    (==) aR1 aR2
        | imp  r1 /= imp r2 = False
        | conC r1 /= conC r2 = False
        | body r1 /= body r2 = False
        | otherwise = True
        where
            r1 = unanonyRule aR1
            r2 = unanonyRule aR2

instance Show AnonyRule where
    show ar = show $ unanonyRule ar 