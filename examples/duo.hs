
{-# LANGUAGE 
    OverloadedStrings, 
    ConstraintKinds,
    NoMonomorphismRestriction #-}

module Main where

import Control.Lens hiding ((|>))
import System.Process (runCommand)
import Music.Prelude.Basic

main = do
    -- writeMidi "test.mid" music
    -- writeXml "test.xml" $ music^/4
    -- openXml music
    openLilypond $ asScore music
    -- playMidiIO "Graphic MIDI" $ music^/10

toLydian :: Score BasicNote -> Score BasicNote
toLydian = pitches' %~ (\p -> if ((p::Behavior Pitch) ! 0) == ((c::Behavior Pitch) ! 0) then cs else p)
-- TODO cleanup

subj1 = (^/2) $
    (legato.accent) (b_ |> c) |> (legato.accent) (c |> b_^*2)
        |> legato (scat [b_, c, d])
        |> b_ |> c |> b_^*2
    |> legato (scat [e, d, b_, c]) |> b_^*2
    |> scat [d, e, b_] |> c^*2 |> b_

pres1 = subj1^*(2/2)
pres2 = subj1^*(2/2) </> delay 2 (subj1^*(3/2))

part1 = pres1 |> pres2
part2 = pres1 |> pres2  

music = asScore $ clef CClef $ level pp $ compress 2 $ part1 |> toLydian part2

-- (|>) :: Score a -> Score a -> Score a
-- a |> b = mcatMaybes $ fmap Just a ||> fmap Just b


