
{-# LANGUAGE TypeFamilies #-}

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
import Music.Score.Combinators (sampleSingle)
import System.Process

main = do
    writeMidi "spanien.mid" score
    writeXml "spanien.xml" $ score^/4
    runCommand "open -a /Applications/Sibelius\\ 6.app spanien.xml"
score = piece

subj1 = ((_p `cresc` mf |> mf `dim` _p)^*(duration subj1' - 2)) `dynamics` subj1'

subj1' :: Score Note
subj1' = (^/2) $
        legato (b_ |> c) |> legato (c |> b_^*2) 
            |> legato (b_ |> c |> d) 
            |> b_ |> c |> b_^*2
        |> legato (e |> d |> b_ |> c) |> b_^*2 
        |> d |> e |> b_ |> c^*2 |> b_

piece = part1 ||> toLydian part2  
    where
        part1 = pres1 ||> pres2 ||> pres3 ||> pres4 ||> pres5 ||> pres6
        part2 = pres1 ||> pres2 ||> pres3 ||> pres4 ||> pres5 ||> pres6
      
pres1 = delay 0 (subj1^*(2/2))
pres2 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres3 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres4 = delay 0 (subj1^*(2/3)) </> delay 2 (subj1^*(3/2))
pres5 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))
pres6 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))


toLydian = modifyPitches (\p -> if p == c then cs else p)











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

asScore :: Score Note -> Score Note
asScore = id

open :: Score Note -> IO ()
open = openXml . (^/4)

play :: Score Note -> IO ()
play = playMidiIO "Graphic MIDI"

simple :: Score (PartT Integer Integer) -> Score (PartT Integer Integer)
simple = id
                                                                 














-- TODO move stuff

resetDynamics :: HasDynamic c => c -> c
resetDynamics = setBeginCresc False . setEndCresc False . setBeginDim False . setEndDim False
-- FIXME setLevel

resetArticulation :: HasArticulation c => c -> c
resetArticulation = setBeginSlur False . setContSlur False . setEndSlur False . setAccLevel 0 . setStaccLevel 0



--------------------------------------------------------------------------------
-- Pitch
--------------------------------------------------------------------------------

-- TODO better transposition etc
-- TODO interval literals (Music.Pitch.Interval.Literal)
up x = fmap (modifyPitch (+ x))
down x = fmap (modifyPitch (subtract x))

-- TODO move to Music.Pitch.Interval.Literal
unison     = 0
octave     = 12
tritone    = 6
fifth      = 7
fourth     = 5
minorThird = 3
majorThird = 4

--------------------------------------------------------------------------------
-- Structure
--------------------------------------------------------------------------------

infixl 6 ||>
a ||> b = padToBar a |> b
bar = rest^*4

padToBar a = a |> (rest ^* (d' * 4))
    where
        d  = snd $ properFraction $ duration a / 4
        d' = if (d == 0) then 0 else (1-d)


rotl []     = []
rotl (x:xs) = xs ++ [x]

rotr [] = []
rotr xs = (last xs:init xs)

rotated n as | n >= 0 = iterate rotr as !! n
             | n <  0 = iterate rotl as !! (abs n)    
             
             

