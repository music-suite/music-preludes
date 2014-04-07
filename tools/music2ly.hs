
module Main where

import           Control.Exception
import           Control.Monad         (when)
import           Data.Version          (showVersion)
-- import           Paths_music_preludes  (version)
import           Data.Char
import           Data.List             (intercalate)
import           Data.List.Split
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe
import           Prelude               hiding (readFile, writeFile)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process
import qualified System.Posix.Env as PE
import           System.IO.Temp

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


{-
  Get the template
  Replace $(prelude) $(main) and $(code)

    prelude = "Music.Prelude.Basic"
    main    = "(openLilypond . asScore)"
    score   = ...

  Write to a temporary .hs file
  Execute
-}

-- TODO
main = do
  args <- getArgs
  main2 args

main2 args = do
  [inFile] <- return args
  let preludeName = "basic"
  let outFile   = takeBaseName inFile ++ ".ly"
  
  let prelude   = "Music.Prelude." ++ toCamel preludeName
  let scoreType = "Score " ++ toCamel preludeName ++ "Note"
  let main      = "writeLilypond"
  score       <- readFile inFile
  newScore    <- return $ expand templ (Map.fromList [
    ("prelude"   , prelude),
    ("main"      , main),
    ("scoreType" , scoreType),
    ("score"     , score),
    ("outFile"   , outFile)
    ])

  withSystemTempDirectory "music2ly." $ \tmpDir -> do
    let tmpFile = tmpDir ++ "/" ++ takeFileName inFile
    putStrLn "Writing..."
    writeFile tmpFile newScore
    withMusicSuiteInScope $ do
      putStrLn "Running..."
      rawSystem "runhaskell" [tmpFile]


  return ()
  where
    templ = "module Main where { import $(prelude); main = $(main) \"$(outFile)\" ( $(score) :: $(scoreType) ) }"


type Template = String

-- |
-- Simple templating system.
-- 
-- @
-- >>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
-- @
-- 
expand :: Template -> Map String String -> String
expand t vs = (composed $ fmap (expander vs) $ Map.keys $ vs) t
  where
    expander vs k = replace ("$(" ++ k ++ ")") (fromJust $ Map.lookup k vs)

composed :: [a -> a] -> a -> a
composed = foldr (.) id


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

toCamel [] = []
toCamel (x:xs) = toUpper x : xs
  
-- | Temporarily modfiy an environment variable (POSIX only).
-- 
-- @
-- withEnv MYVAR (\oldValue -> newValue) $ do
--    ...
-- @
-- 
withEnv :: String -> (Maybe String -> String) -> IO a -> IO ()
withEnv n f k = do
  x <- PE.getEnv n
  PE.setEnv n (f x) True
  k
  case x of
    Nothing -> PE.unsetEnv n
    Just x2 -> PE.setEnv n x2 True  

-- TODO if no music-util, use ""
withMusicSuiteInScope k = do
  packagePath <- readProcess "music-util" ["package-path"] ""
  withEnv "GHC_PACKAGE_PATH" (const packagePath) k
