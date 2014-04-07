
module Main where

import           Music.Prelude.CmdLine
import           System.Environment

-- TODO
main = do
  args <- getArgs
  main2 args

main2 args = do
  [inFile] <- return args
  translateFileAndRunLilypond "pdf" (Just "basic") (Just inFile)
