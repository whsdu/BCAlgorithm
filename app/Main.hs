module Main where
import System.Environment
import System.Exit

import qualified Space.Language as L 
import qualified Utility.Ordering as O 
import qualified Utility.Paths as Ps
import Parser.FileParser
    ( parseEnv,
      parseLiteralMap,
      parsePreferenceMap,
      parseQueryLiteral )
import Env 
import Data.List (intercalate)

main = do 
    args <- getArgs 
    if length args /= 3
        then usage >> exit
        else search args
search [file,query,orderings] = do 
    envInit <- parseEnv file
    let 
        ord = 
            case orderings of 
                "weakest-dem" -> O.weakestDem
                "last-eli" -> O.lastEli 
                "last-dem" -> O.lastDem 
                _ -> O.weakestEli 
        qm = parseLiteralMap envInit
        a0 = parseQueryLiteral query qm
        env = envInit{envOrdering = ord}
        run = runApp env
    r <- run $ Ps.query a0
    putStrLn . showGroup  $ r 

showGroup :: [L.EquifinalPaths] -> String
showGroup group = intercalate "\n" $ show <$> concat group 

usage :: IO ()
usage   = putStrLn "Usage: DT-exe fileName query"
exit :: IO a
exit    = exitSuccess