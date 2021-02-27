module Arena.AttackGraph1 where 

import           qualified Data.HashMap.Strict as Map 
import           qualified Space.Argumentation as A 
import           qualified Space.Language   as L 
import           qualified Parser.FileParser as PF
import           qualified Space.Meta as M 




-- | Adopt graph in Graph4 as the one that equivalent to the target Argument. 
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

testGraph4 = 
    [ ra1
    , rb1,rb2,rb3
    , rc1,rc2,rc3,rc4,rc5 
    , rd1,rd2
    , re1
    ]

