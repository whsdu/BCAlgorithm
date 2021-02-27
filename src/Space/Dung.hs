-- {-# LANGUAGE ExistentialQuantification #-}
module Space.Dung where 

type AF = ([AR],Attacks)

-- data AR = forall a . Eq a => AR {getAr :: a}
type AR = String 
type Attack = (AR,AR)
type Attacks = [Attack]

demoARs = ["A","B","C", "D", "E", "F", "L", "G", "O"]

demoAttacks = 
    [ ("A", "B")
    , ("L", "A")
    , ("B", "C")
    , ("C", "F")
    , ("F", "D")
    , ("B", "E")
    , ("E", "G")
    , ("G", "O")
    , ("O", "D")
    ]

testARs :: [[Char]]
testARs = ["A","B","C", "D", "E", "F","G"]
testAttacks :: [([Char], [Char])]
testAttacks = 
    [ ("A", "B")
    , ("B", "A")
    , ("A", "C")
    , ("B", "C")
    , ("C", "D")
    , ("D", "E")
    , ("G", "E")
    , ("E", "F")
    ]

cicleARs :: [String]
cicleARs = ["A","B"]
cicleAttacks :: [([Char], [Char])]
cicleAttacks = 
    [ ("A","B")
    , ("B","A")
    ]

demoAF :: ([[Char]], [([Char], [Char])])
demoAF = (demoARs,demoAttacks)

testAF :: ([[Char]], [([Char], [Char])])
testAF = (testARs, testAttacks)

cicleAF :: ([String], [([Char], [Char])])
cicleAF = (cicleARs, cicleAttacks)


isAttack :: (Eq b, Foldable t1, Foldable t2) => (t1 b, t2 (b, b)) -> b -> b -> Bool
isAttack (ars, attacks) a b = 
    a `elem` ars &&
    b `elem` ars &&
    (a,b) `elem` attacks

-- | argument set s attack argument a 
isSetAttack :: (Functor t, Eq b, Foldable t, Foldable t1, Foldable t2) => (t1 b, t2 (b, b)) -> t b -> b -> Bool
isSetAttack af s b = or $ flip (isAttack af) b <$> s

-- | Any two elements of argument set sars do not attack each other
isConflictFree af sars = 
    not $ or  
            [ isAttack af a b 
            | a <- sars
            , b <- sars 
            ] 

isAcceptable af@(ars,_) [] a = 
    not $ isSetAttack af ars a  
isAcceptable af@(ars, attacks) s a =                    
    let 
        attackers = [ b | b <- ars, (b,a) `elem` attacks]
    in 
        if attackers == [] 
            then True                                   
            else and [ or (flip (isAttack af) a <$> s) | a <- attackers]

isAdmissible af s = 
        isConflictFree af s 
        &&
        and [isAcceptable af s a | a <- s]

faf :: (Foldable t, Eq a) => ([a], t (a, a)) -> [a] -> [a]
faf af@(ars, _) supportARs = [ a | a <- ars, isAcceptable af supportARs a]

