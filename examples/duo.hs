
module Main where

import System.Process (runCommand)
import Music.Prelude.Basic

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10

score = piece

subj1 = ((_p `cresc` mf |> mf `dim` _p)^*(duration subj1' - 2)) `dynamics` subj1'

subj1' :: Score Note
subj1' = (^/2) $
        legato (b_ |> c) |> legato (c |> b_^*2) 
            |> legato (b_ |> c |> d) 
            |> b_ |> c |> b_^*2
        |> legato (e |> d |> b_ |> c) |> b_^*2 
        |> d |> e |> b_ |> c^*2 |> b_

piece = part1 |> toLydian part2  
    where
        part1 = pres1 |> pres2 |> pres3 |> pres4 |> pres5 |> pres6
        part2 = pres1 |> pres2 |> pres3 |> pres4 |> pres5 |> pres6
      
pres1 = delay 0 (subj1^*(2/2))
pres2 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres3 = delay 0 (subj1^*(2/2)) </> delay 2 (subj1^*(3/2))
pres4 = delay 0 (subj1^*(2/3)) </> delay 2 (subj1^*(3/2))
pres5 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))
pres6 = delay 0 (subj1^*(2/3)) </> delay 4 (subj1^*(2/2))


toLydian = modifyPitches (\p -> if p == c then cs else p)

-- (|>) :: Score a -> Score a -> Score a
-- a |> b = mcatMaybes $ fmap Just a ||> fmap Just b


