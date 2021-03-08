module Main where
import System.Environment
import System.Exit

import qualified Utility.Ordering as O 
import Parser.FileParser
    ( parseEnv,
      parseLiteralMap,
      parsePreferenceMap,
      parseQueryLiteral )
import EnvDef 
import Data.List (intercalate)

main = undefined 