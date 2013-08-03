
import Control.Exception
import Control.Monad (when)
-- import Control.Monad.Error hiding (mapM)
-- import Control.Monad.Plus hiding (mapM)
-- import Data.Semigroup hiding (Option)
-- import Data.List (find)
-- import Data.Maybe (fromMaybe, maybeToList)
-- import Data.Traversable (mapM)
-- import Data.Typeable
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Prelude hiding (readFile, writeFile)

main :: IO ()
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo (header "music2ly") options
    let printUsage   = putStr (usage ++ "\n")   >> exitSuccess
    let printVersion = putStr (version "music2ly" ++ "\n") >> exitSuccess

    when (1 `elem` opts) printUsage
    when (2 `elem` opts) printVersion
    printVersion

version name = name ++ "-0.8"
header  name = "Usage: "++name++" [options]\n" ++
               "Usage: "++name++" [options] files...\n" ++
               "\n" ++
               "Options:"

options = [
    Option ['h'] ["help"]          (NoArg 1)   "Print help and exit",
    Option ['v'] ["version"]       (NoArg 2)   "Print version and exit"
  ]


-- Load the file into the interpreter
-- Get the 'score' variable
-- Run (writeLy path score)