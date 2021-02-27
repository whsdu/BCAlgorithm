{-# LANGUAGE GADTs #-}
module Space.Argumentation
    ( Argumentation(..)
    , Preference(..)
    , ArgumentationSpace
    , PreferenceSpace
    )where

import Space.Meta 
import qualified Space.Language as L

data Argumentation = Argumentation
    { name :: Name
    , body :: [Argumentation]
    , imp  :: Imp
    , conC :: L.Literal
    }

data Preference where
    Prefer :: Argumentation -> Argumentation -> Preference 

type ArgumentationSpace = [Argumentation]
type PreferenceSpace = [Preference]

instance Show Argumentation where
    show (Argumentation n b i c) =
        n ++ " :" ++ body ++ imp ++ head
        where
            body = unwords $ name <$> b
            imp = show i
            head = L.name c

instance Eq Argumentation where
    (==) a1 a2 = name a1 == name a2


instance Show Preference where
    show (Prefer a1 a2) = name a1 ++ " > " ++ name a2

instance Eq Preference where
    (==) (Prefer pr11 pr12) (Prefer pr21 pr22)
        | pr11 /= pr21 = False
        | pr12 /= pr22 = False
        | otherwise = True