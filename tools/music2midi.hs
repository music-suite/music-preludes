
module Main where

import           Data.Monoid
import           Music.Prelude.CmdLine
import           Options.Applicative
import           System.Environment

data Options = Options {
    prelude :: Maybe String,
    outFile :: Maybe FilePath,
    inFile  :: FilePath
  } deriving (Show)

options :: Parser Options
options = Options
  <$> (optional $ strOption $ mconcat [long "prelude", metavar "<name>"])
  <*> (optional $ strOption $ mconcat [short 'o', long "output", metavar "<file>"])
  <*> (argument str $ metavar "<input>")

run :: Options -> IO ()
run (Options prelude outFile inFile) =
  translateFile "writeMidi" "mid" prelude (Just inFile) outFile

main :: IO ()
main = do
  pgmName <- getProgName
  let opts = info (helper <*> options) (fullDesc <> header (pgmName ++ "-" ++ versionString))
  execParser opts >>= run
