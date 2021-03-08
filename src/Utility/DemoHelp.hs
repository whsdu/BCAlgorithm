{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}
module Utility.DemoHelp where 


import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)

import EnvDef ( grab, runApp,App, Has, UseRuleOnly ) 

import qualified Space.Defeasible as D 
import qualified Space.Meta as M 

import qualified Parser.FileParser as FP



getRunner :: FilePath -> App b -> IO b
getRunner filePath func = do 
    env <- FP.parseEnv filePath 
    runApp env func 

bugRunner :: FilePath -> IO (App a -> IO a)
bugRunner filePath = do 
    env <- FP.parseEnv filePath 
    pure $ runApp env 

getProposition :: FilePath -> String  -> IO D.Literal 
getProposition filePath pro = do 
    env <- FP.parseEnv filePath
    let 
        runner = runApp env
        lMap = FP.parseLiteralMap env
    pure $ FP.parseQueryLiteral pro lMap