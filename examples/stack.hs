
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Control.Applicative
import Control.Apply.Reverse
import Data.Semigroup
import Data.VectorSpace
import Data.AffineSpace
import Data.Foldable (Foldable(..), toList)
import Data.Ord (comparing)
import qualified Data.List as List

import Music.Pitch.Literal
import Music.Dynamics.Literal
import Music.Score
import Music.Score.Zip
import System.Process

main = do
    writeMidi "test.mid" score
    writeXml "test.xml" $ score^/4
    runCommand "open -a /Applications/Sibelius\\ 6.app test.xml"
score = asScore $ mempty




data NotePart
    = Vl1
    | Vl2
    deriving (Eq, Ord, Enum)

instance Show NotePart where
    show Vl1  = "Violin 1"
    show Vl2  = "Violin 2"

vl1, vl2 :: NotePart
vl1  = Vl1
vl2  = Vl2

-- type Note = (PartT NotePart (TieT
--     (TremoloT (HarmonicT (SlideT
--         (DynamicT (ArticulationT (TextT Integer))))))))
type Note = PartT Int (PartT () Integer)

asScore :: Score Note -> Score Note
asScore = id




