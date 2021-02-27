module Utility.Query where 

-- This module is the rewrite of Path 

-- | The semantic of l = [...] is : 
-- 1. many necessary of l : paths in EFP, path-sections in Path. 
-- 2. many options contained in l , this usually indicate  2 options: 
--      1. f <$> l :  options are treated by f equally. 
--      2. concat $ f <$> l : results of above function are treated equally. 