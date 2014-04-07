
module Music.Prelude.CmdLine where

-- import           Data.Version          (showVersion)
-- import           Paths_music_preludes  (version)
import           Data.Char
import           Data.List          (intercalate)
import           Data.List.Split
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe
import           Prelude            hiding (readFile, writeFile)
-- import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
import qualified System.Posix.Env   as PE
import           System.Process

translateFileAndRunLilypond :: String -> Maybe String -> Maybe FilePath -> IO ()
translateFileAndRunLilypond format preludeName' inFile' = do
  let inFile      = fromMaybe "test.music" inFile'
  let preludeName = fromMaybe "basic" preludeName'
  let lyFile      = takeBaseName inFile ++ ".ly"

  translateFile "writeLilypond" "ly" (Just preludeName) (Just inFile) (Just lyFile)
  rawSystem "lilypond" ["--" ++ format, "-o", takeBaseName inFile, lyFile]
  runCommand $ "rm -f " ++ takeBaseName inFile ++ "-*.tex " ++ takeBaseName inFile ++ "-*.texi " ++ takeBaseName inFile ++ "-*.count " ++ takeBaseName inFile ++ "-*.eps " ++ takeBaseName inFile ++ "-*.pdf " ++ takeBaseName inFile ++ ".eps"
  return ()

translateFile :: String -> String -> Maybe String -> Maybe FilePath -> Maybe FilePath -> IO ()
translateFile translationFunction outSuffix preludeName' inFile' outFile' = do
  let inFile      = fromMaybe "test.music" inFile'
  let preludeName = fromMaybe "basic" preludeName'
  let outFile     = fromMaybe (takeBaseName inFile ++ "." ++ outSuffix) outFile'

  let prelude   = "Music.Prelude." ++ toCamel preludeName
  let scoreType = "Score " ++ toCamel preludeName ++ "Note"
  let main      = translationFunction
  score       <- readFile inFile
  newScore    <- return $ expand templ (Map.fromList [
    ("prelude"   , prelude),
    ("main"      , main),
    ("scoreType" , scoreType),
    ("score"     , score),
    ("outFile"   , outFile)
    ])

  withSystemTempDirectory "music-suite." $ \tmpDir -> do
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
