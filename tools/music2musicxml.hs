
module Main where

import           Music.Prelude.CmdLine
import           System.Environment

main :: IO ()
main = do
  pgmName <- getProgName
  converterMain "writeMusicXml" "xml" pgmName
