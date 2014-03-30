
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Process (runCommand)
import Music.Prelude.Basic

{-    
    Franz Schubert: Erlkönig
-}

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLilypond score
    -- playMidiIO "Graphic MIDI" $ score^/10

score = let
        meta = id
            . title "Erlkönig, op.1  (excerpt)"
            . composer "Franz Schubert"
            . timeSignature (4/4)
            . keySignature (key g False)
        
        triplet = group 3

        a `x` b = a^*(3/4) |> b^*(1/4)
        a `l` b = (a |> b)^/2
    
        motive = (legato $ stretchTo 2 $ scat [g,a,bb,c',d',eb']) |> staccato (d' |> bb |> g)
        bar    = rest^*4

        song    = mempty
        left    = below _P8 $ times 4 (times 4 $ removeRests $ triplet g)
        right   = removeRests $ clef FClef $ down _P8 $ times 2 (delay 4 motive |> rest^*3)

        -- Use 4/4 or 12/8 notation
        useCommonTime = True
        scale         = if useCommonTime then id else timeSignature (time 12 8) . stretch (3/2) 
        below a x     = x <> down a x

    in asScore $ meta $ scale $ stretch (1/4) $ song </> left </> down _P8 right