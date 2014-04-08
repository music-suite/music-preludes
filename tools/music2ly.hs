
module Main where

import           Music.Prelude.CmdLine
import           System.Environment

main :: IO ()
main = getProgName >>= converterMain "writeLilypond" "ly"