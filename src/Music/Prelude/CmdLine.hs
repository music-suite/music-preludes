
module Music.Prelude.CmdLine (
        converterMain,
        translateFileAndRunLilypond,
        translateFile,
        versionString
) where

import           Control.Exception
import           Data.Version          (showVersion)
import           Data.Monoid
import           Options.Applicative
-- import           Paths_music_preludes  (version)
import           Data.Char
import           Data.List          (intercalate, isPrefixOf)
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

-- TODO Can not link due to haskell/cabal#1759
versionString :: String
versionString = "1.7"
-- versionString = showVersion version

data Options = Options {
    prelude :: Maybe String,
    outFile :: Maybe FilePath,
    inFile  :: FilePath
  } deriving (Show)

converterOptions :: Parser Options
converterOptions = Options
  <$> (optional $ strOption $ mconcat [long "prelude", metavar "<name>"])
  <*> (optional $ strOption $ mconcat [short 'o', long "output", metavar "<file>"])
  <*> (argument str $ metavar "<input>")

runConverter :: String -> String -> Options -> IO ()
runConverter func ext (Options prelude outFile inFile) =
  translateFile func ext prelude (Just inFile) outFile

converterMain :: String -> String -> IO ()
converterMain func ext = getProgName >>= converterMain' func ext

converterMain' :: String -> String -> String -> IO ()
converterMain' func ext pgmName = execParser opts >>= runConverter func ext
  where 
      opts = info 
        (helper <*> converterOptions) 
        (fullDesc <> header (pgmName ++ "-" ++ versionString))


translateFileAndRunLilypond :: String -> Maybe String -> Maybe FilePath -> IO ()
translateFileAndRunLilypond format preludeName' inFile' = do
  let inFile      = fromMaybe "test.music" inFile'
  let preludeName = fromMaybe "basic" preludeName'
  let lyFile      = takeBaseName inFile ++ ".ly"

  translateFile "writeLilypond" "ly" (Just preludeName) (Just inFile) (Just lyFile)
  rawSystem "lilypond" ["--" ++ format, "-o", takeBaseName inFile, lyFile]
  runCommand $ "rm -f " ++ takeBaseName inFile ++ "-*.tex " ++ takeBaseName inFile ++ "-*.texi " ++ takeBaseName inFile ++ "-*.count " ++ takeBaseName inFile ++ "-*.eps " ++ takeBaseName inFile ++ "-*.pdf " ++ takeBaseName inFile ++ ".eps"
  return ()

translateFile 
  :: String         -- ^ Translate function (of type FilePath -> music -> IO ()).
  -> String         -- ^ Output file suffix.
  -> Maybe String   -- ^ Prelude to use.
  -> Maybe FilePath -- ^ Input file.
  -> Maybe FilePath -- ^ Output file.
  -> IO ()
translateFile translationFunction outSuffix preludeName' inFile' outFile' = do
  let inFile      = fromMaybe "test.music" inFile'
  let preludeName = fromMaybe "basic" preludeName'
  let outFile     = fromMaybe (
                    takeDirectory inFile ++ "/" 
                    ++ takeBaseName inFile 
                    ++ "." ++ outSuffix) 
                    outFile'

  let prelude   = "Music.Prelude." ++ toCamel preludeName
  let scoreType = "Score " ++ toCamel preludeName ++ "Note"
  let main      = translationFunction
  code          <- readFile inFile
  newScore      <- return $ if isNotExpression code 
    then expand declTempl (Map.fromList [
      ("prelude"   , prelude),
      ("main"      , main),
      ("scoreType" , scoreType),
      ("code"      , code),
      ("outFile"   , outFile)
      ])
    else expand exprTempl (Map.fromList [
      ("prelude"   , prelude),
      ("main"      , main),
      ("scoreType" , scoreType),
      ("score"     , code),
      ("outFile"   , outFile)
      ])
  -- putStrLn newScore
  withSystemTempDirectory "music-suite." $ \tmpDir -> do
    let tmpFile = tmpDir ++ "/" ++ takeFileName inFile
    let opts = ["-XOverloadedStrings", "-XNoMonomorphismRestriction", "-XTypeFamilies"]
    putStrLn $ "Converting music..."
    writeFile tmpFile newScore
    withMusicSuiteInScope $ do
      putStrLn $ "Writing '" ++ outFile ++ "'..."
      rawSystem "runhaskell" (opts <> [tmpFile])  >>= \e -> if e == ExitSuccess then return () else fail ("Could not convert"++inFile)

  return ()
  where
    exprTempl = "module Main where { import $(prelude); main = $(main) \"$(outFile)\" ( $(score) :: $(scoreType) ) }"
    declTempl = "module Main where \nimport $(prelude) \n$(code) \nmain = $(main) \"$(outFile)\" ( example  :: $(scoreType) )"

-- TODO hackish, preferably parse using haskell-src-exts or similar
isNotExpression :: String -> Bool
isNotExpression t = anyLineStartsWith "type" t || anyLineStartsWith "data" t || anyLineStartsWith "example =" t



-- |
-- >>> anyLineStartsWith "h" "ahc"
-- False
-- >>> anyLineStartsWith "h" "a\nhc"
-- True
-- >>> anyLineStartsWith "h" "hac"
--
anyLineStartsWith :: String -> String -> Bool
anyLineStartsWith t = any (t `isPrefixOf`) . lines


type Template = String

-- |
-- Simple templating system.
--
-- >>> expand "me : $(name)" (Map.fromList [("name","Hans")])
-- "me : Hans"
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
withEnv :: String -> (Maybe String -> String) -> IO a -> IO a
withEnv n f k = do
  x <- PE.getEnv n
  PE.setEnv n (f x) True
  res <- k
  case x of
    Nothing -> PE.unsetEnv n >> return res
    Just x2 -> PE.setEnv n x2 True >> return res

withMusicSuiteInScope :: IO a -> IO a
withMusicSuiteInScope k = do
  r <- try $ readProcess "music-util" ["package-path"] ""
  case r of
    Left x            -> let _ = (x::SomeException) in withEnv "GHC_PACKAGE_PATH" (const "") k
    Right packagePath -> withEnv "GHC_PACKAGE_PATH" (const packagePath) k
