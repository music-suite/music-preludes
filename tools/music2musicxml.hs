
module Main where

import           Music.Prelude.CmdLine
import           System.Environment

{-
main :: IO ()
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs
    prg <- getProgName

    let usage        = usageInfo (header "music2ly") options
    let printUsage   = putStr (usage ++ "\n")   >> exitSuccess
    let printVersion = putStr (prg ++ "-" ++ showVersion version ++ "\n") >> exitSuccess

    when (1 `elem` opts) printUsage
    when (2 `elem` opts) printVersion
    printVersion

header  name = "Usage: "++name++" [options]\n" ++
               "Usage: "++name++" [options] files...\n" ++
               "\n" ++
               "Options:"

options = [
    Option ['h'] ["help"]          (NoArg 1)   "Print help and exit",
    Option ['v'] ["version"]       (NoArg 2)   "Print version and exit"
  ]

-}

-- TODO parse options
main = getArgs >>= main2

main2 args = do
  let [inFile] = args
  translateFile "writeMusicXml" "xml" (Just "basic") (Just inFile) Nothing
