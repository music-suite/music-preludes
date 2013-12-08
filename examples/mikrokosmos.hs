
import System.Process (runCommand)
import Music.Prelude.Basic

-- | Bela Bartok – “Wandering” from Mikrokosmos, volume III
--   This example was adaptd from Abjad

main = do
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10

score :: Score Note
score = let
        left = (dynamics pp . legato) 
               (scat [a,g,f,e] |> d^*2)
            |> (dynamics ((mp |> mp `cresc` mf |> mf)^*8) . legato) 
               (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)
        right = up _P4 . delay 2 $ 
               (dynamics pp . legato) 
               (scat [a,g,f,e] |> d^*2)
            |> (dynamics mp . legato) 
               (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)

    in  (^*(1/8)) $ left </> down _P8 right
