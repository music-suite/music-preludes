
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import System.Process (runCommand)
import Music.Prelude.Basic

main = do
    writeMidi "test.mid" score
    writeXml "test.xml" $ score^/4
    -- writeLy "test.ly" $ score^/4
    -- writeAbc "test.abc" $ score^/4
    -- writeGuido "test.gmn" $ score^/4
    runCommand "open -a /Applications/Sibelius\\ 6.app test.xml"
score = piece

subj1 = ((_p `cresc` mf |> mf `dim` _p)^*(duration subj1' - 2)) `dynamics` subj1'

subj1' :: Score Note
subj1' = (^/2) $
        legato (b_ |> c) |> legato (c |> b_^*2) 
            |> legato (b_ |> c |> d) 
            |> b_ |> c |> b_^*2
        |> legato (e |> d |> b_ |> c) |> b_^*2 
        |> d |> e |> b_ |> c^*2 |> b_

piece = part1 !!> toLydian part2  
    where
        part1 = pres1 !!> pres2 !!> pres3 !!> pres4 !!> pres5 !!> pres6
        part2 = pres1 !!> pres2 !!> pres3 !!> pres4 !!> pres5 !!> pres6
      
pres1 = delay 0 (subj1^*(2/2))
pres2 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres3 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres4 = delay 0 (subj1^*(2/3)) </> delay 2 (subj1^*(3/2))
pres5 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))
pres6 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))


toLydian = modifyPitches (\p -> if p == c then cs else p)

(!!>) :: Score a -> Score a -> Score a
a !!> b = mcatMaybes $ fmap Just a ||> fmap Just b










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
                                                                 








--------------------------------------------------------------------------------
-- Pitch
--------------------------------------------------------------------------------

-- TODO better transposition etc
-- TODO interval literals (Music.Pitch.Interval.Literal)
up x = fmap (modifyPitch (+ x))
down x = fmap (modifyPitch (subtract x))

up :: (Functor f, Num (Pitch b), HasPitch b) => Pitch b -> f b -> f b

upOctave :: (Functor f, HasPitch b, Num (Pitch b)) => f b -> f b
upOctave = up octave
  
-- TODO move to Music.Pitch.Interval.Literal
unison     = 0
octave     = 12
tritone    = 6
fifth      = 7
fourth     = 5
minorThird = 3
majorThird = 4

