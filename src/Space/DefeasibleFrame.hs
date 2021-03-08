{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Space.DefeasibleFrame where 

class Attack a c where 
    conflict :: a -> a -> c 


class Engage a where 
    defeat :: a -> a -> Bool 


