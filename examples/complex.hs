
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
    writeMidi "complex.mid" score
    writeXml "complex.xml" $ score^/4
    runCommand "open -a /Applications/Sibelius\\ 6.app complex.xml"


infixr 7 //
(//) = flip repTimes

score = (c^*(1/5)) // 4 |> d^*(1/5+1/2) |> e^*(1/2)









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

type Note = (PartT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))

score :: Score Note

asScore :: Score Note -> Score Note
asScore = id

open :: Score Note -> IO ()
open = openXml . (^/4)

play :: Score Note -> IO ()
play = playMidiIO "Graphic MIDI"

simple :: Score (PartT Integer Integer) -> Score (PartT Integer Integer)
simple = id
                                                                 













