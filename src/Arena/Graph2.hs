module Arena.Graph2
    ( testGraph2
    )
    where 

import           qualified Data.HashMap.Strict as Map 
import           qualified Space.Argumentation as A 
import           qualified Space.Language   as L 
import           qualified Parser.FileParser as PF
import           qualified Space.Meta as M 

a1 :: L.Literal
a1 = L.Atom "a1"

b1 = L.Atom "b1"
b2 = L.Atom "b2"
b3 = L.Atom "b3"

c1 = L.Atom "c1"
c2 = L.Atom "c2"
c3 = L.Atom "c3"
c4 = L.Atom "c4"
c5 = L.Atom "c5"

-- d1 = L.Atom "d1"
-- e1 = L.Atom "e1"

ra1 = L.Rule "ra1" [b1,b2,b3] M.S a1 

rb1 = L.Rule "rb1" [c1,c2] M.S b1 
rb2 = L.Rule "rb2" [c3] M.S b2 
rb3 = L.Rule "rb3" [c4,c5] M.S b3 

rc1 = L.Rule "rc1" [] M.S c1
rc2 = L.Rule "rc2" [] M.S c2
rc3 = L.Rule "rc3" [] M.S c2
rc4 = L.Rule "rc4" [] M.S c3
rc5 = L.Rule "rc5" [] M.S c4
rc6 = L.Rule "rc6" [] M.S c5
rc7 = L.Rule "rc7" [] M.S c5

testGraph2 = 
    [ ra1
    ,rb1,rb2,rb3
    ,rc1,rc2,rc3,rc4,rc5,rc6,rc7]

