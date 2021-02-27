module Arena.GraphSimpleDemo
    where 

import           qualified Data.HashMap.Strict as Map 
import           qualified Space.Argumentation as A 
import           qualified Space.Language   as L 
import           qualified Parser.FileParser as PF
import           qualified Space.Meta as M 


aSD = L.Atom "a"
-- rSD = L.Rule "rSD" [a1,f1] M.S aSD
rSD1 = L.Rule "rSD1" [a1] M.S aSD
rSD2 = L.Rule "rSD2" [f1] M.S aSD

-- demoGraph = [rSD] ++ subGraph1 ++ subGraph2
demoGraph = [rSD1,rSD2] ++ subGraph1 ++ subGraph2


---- tow subgraphs
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

subGraph1 = 
    [ ra1
    , rb1,rb2,rb3
    , rc1,rc2,rc3,rc4,rc5 
    , rd1,rd2
    , re1
    ]

--  =========
-- a to f 
f1 :: L.Literal
f1 = L.Atom "f1"
-- b to g
g1 = L.Atom "g1"
g2 = L.Atom "g2"
g3 = L.Atom "g3"
-- c to h
h1 = L.Atom "h1"
h2 = L.Atom "h2"
h3 = L.Atom "h3"
-- d to k
k1 = L.Atom "k1"

rf1 = L.Rule "rf1" [g1,g2,g3] M.S f1 

rg1 = L.Rule "rg1" [h1,h2] M.S g1 
rg2 = L.Rule "rg2" [] M.S g2 
rg3 = L.Rule "rg3" [h3] M.S g3 

rh1 = L.Rule "rh1" [] M.S h1
rh2 = L.Rule "rh2" [] M.S h2
rh3 = L.Rule "rh3" [k1] M.S h2
rh4 = L.Rule "rh4" [] M.S h3
rk1 = L.Rule "rk1" [] M.S k1 
rk2 = L.Rule "rk2" [] M.S k1 

subGraph2= 
    [rf1
    ,rg1,rg2,rg3
    ,rh1,rh2,rh3,rh4
    ,rk1,rk2]

