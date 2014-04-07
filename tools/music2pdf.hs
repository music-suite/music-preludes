
import Control.Exception
import Control.Monad (when)
-- import Control.Monad.Error hiding (mapM)
-- import Control.Monad.Plus hiding (mapM)
-- import Data.Semigroup hiding (Option)
-- import Data.List (find)
-- import Data.Maybe (fromMaybe, maybeToList)
-- import Data.Traversable (mapM)
-- import Data.Typeable
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process

-- TODO
main = do
  args <- getArgs
  main2 args

main2 args = do
    [inFile] <- return args
    rawSystem "music2ly" [inFile] -- TODO pass outfile    
    rawSystem "lilypond" ["-o", takeBaseName inFile ++ ".pdf", takeBaseName inFile ++ ".ly"]