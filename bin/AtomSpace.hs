module AtomSpace 
    (
        Atom (..)
    )where 

import qualified MetaDefinition as M 

newtype Atom = Atom {atomName :: M.Name} 

instance M.Literalable Atom where 
    literal = atomName

instance Show Atom where 
    show (Atom n) = n 

instance Eq Atom where 
    (==) a1 a2 = atomName a1 == atomName a2 