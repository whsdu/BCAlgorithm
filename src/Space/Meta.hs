module Space.Meta where 

import Data.List (group, sort)

type Name = String 

data Imp = S | D | N

instance Show Imp where 
    show S = "->"
    show D = "=>"
    show N = " "

instance Eq Imp where 
    (==) S S = True 
    (==) D D = True
    (==) N N = True 
    (==) _ _ = False

class (Eq a) => Negation a where 
    neg :: a -> a 
    negation :: a -> a -> Bool 
    negation a1 a2 = neg a1 == a2

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- | This class is not necessary due to the nature of type Literal 
-- ** Atom is actually indistinguishable from Rule !!!
-- class (Show a, Negation a) => Literalable a where 
--     literal :: a -> Name 
--     ruleBody :: a -> [a] 
--     ruleImp :: a -> Imp 
--     ruleHead :: a -> a 