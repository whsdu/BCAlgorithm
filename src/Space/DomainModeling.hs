{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Space.DomainModeling where 

class RS a b  where 
    f :: a -> a -> b 

class RS2 b where 
    p :: b -> String 

class (RS a b, RS2 b) => Compose a b where 
    c :: a -> b -> String 

newtype Long = Long [String]

data SE = SE 
    { d1 :: Int 
    , d2 :: Double 
    , d3 :: Long
    , f2 :: forall a b . RS a b => a -> a -> b
    }

class Has a b where 
    grab :: a -> b 

instance Has SE Int where 
    grab = undefined 

instance Has SE Double where 
    grab = undefined 

instance Has SE Long  where 
    grab = undefined 

type HasEnv = (Has SE Int, Has SE Double, Has SE Long)

instance RS Int Bool where 
    f i1 i2 = i1 == i2 


class Attack1 a b c | a b -> c where 
    convert :: SE -> a 
    attack :: a -> b -> c 

