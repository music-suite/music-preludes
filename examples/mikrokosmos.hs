
import System.Process (runCommand)
import Music.Prelude.Basic

-- | Bela Bartok – “Wandering” from Mikrokosmos, volume III
--   This example was adaptd from Abjad

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml $ score
    writeLy "test.ly" $ score
    runCommand "lilypond test.ly"
    -- playMidiIO "Graphic MIDI" $ score^/10

score :: Score Note
score = let
        up x = fmap (modifyPitch (+ x))
        down x = fmap (modifyPitch (subtract x))
        octave = 12
        fourth = 5

        left = (dynamics pp . legato) 
               (melody [a,g,f,e] |> d^*2)
            |> (dynamics ((mp |> mp `cresc` mf |> mf)^*8) . legato) 
               (melody [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)
        right = up fourth . delay 2 $ 
               (dynamics pp . legato) 
               (melody [a,g,f,e] |> d^*2)
            |> (dynamics mp . legato) 
               (melody [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)

    in  (^*(1/8)) $ left </> down octave right
