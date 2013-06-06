
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
    writeLy "test.ly" $ score^/4
    runCommand "lilypond -f png test.ly"
    -- runCommand "open -a /Applications/Sibelius\\ 6.app test.xml"


-- infixr 7 //
-- (//) = flip repTimes

score = test 1 </> test 2

test 1  = group 5 c |> c^*3                  
test 2  = group 3 c |> c^*3                  
test 3  = group 3 c |> group 5 c |> group 3 c |> group 7 c
test 4  = group 3 c |> group 5 c |> c |> group 7 c
test 5  = c |> group 5 c |> c |> group 7 c
-- all above ok


test 8 = repTimes 5 c^/5 |> repTimes 3 c^/3 -- ok
test 9 = repTimes 4 c^/5 |> c^*(1/5+1/3)    -- not ok, needs to be bound from last quintuplet note

test 99 = group 5 c |> group 3 c |> c^*2








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
                                                                 













