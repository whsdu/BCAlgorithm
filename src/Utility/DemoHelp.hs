{-# Language TypeApplications #-}
{-# Language FlexibleContexts #-}
module Utility.DemoHelp where 


import Control.Monad.IO.Class (MonadIO) 
import Control.Monad.Reader (MonadReader)

import Env ( grab, runApp,App, Has, UseRuleOnly ) 

import qualified Space.Language as L 
import qualified Space.Meta as M 

import qualified Utility.Language as LU 
import qualified Utility.Path as PS
import qualified Utility.Ordering  as O
import qualified Parser.FileParser as FP

{--}
rebutDemo' :: 
    ( MonadReader env m 
    , Has L.Language env 
    , UseRuleOnly env 
    , MonadIO m
    ) => L.Path -> m [(L.Literal,L.EquifinalPaths)]
rebutDemo' path = do 
    lang <- grab @L.Language
    let 
        defeasible = [r | r <- concat path , L.imp r == M.D]
        rebutPoints = L.conC <$> defeasible
    attackEFP <- mapM PS.querySingleConclusion ( M.neg <$> rebutPoints)
    let 
        r = zip rebutPoints attackEFP
    pure  [l | l <- r, snd l /= []]

orderingHelper :: FilePath  -> String -> IO (L.Path, L.Path)
orderingHelper filePath pro = do 
    env <- FP.parseEnv filePath
    let 
        runner = runApp env
        lMap = FP.parseLiteralMap env
        query = FP.parseQueryLiteral pro lMap
    queryEFP <- runner $ PS.querySingleConclusion query 
    r <- runner $ rebutDemo' (head queryEFP)
    let 
        attackPoint = fst . head $ r 
        attackPath = head . snd . head $ r 
    targetPath <- fmap head $ runner $ PS.querySingleConclusion attackPoint
    pure (attackPath, targetPath) 

getRunner :: FilePath -> App b -> IO b
getRunner filePath func = do 
    env <- FP.parseEnv filePath 
    runApp env func 

bugRunner :: FilePath -> IO (App a -> IO a)
bugRunner filePath = do 
    env <- FP.parseEnv filePath 
    pure $ runApp env 