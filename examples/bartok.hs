
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude.Basic
import System.Process (system)

{-    
    Bela Bartok: Wandering from Mikrokosmos, volume III
    Inspired by the Abjad transcription
-}

main = openLilypond score

score :: Score Note
score = let
        meta = id
            . title "Mikrokosmos (excerpt)"
            . composer "Bela Bartok"
            . timeSignatureDuring ((2/4) >-> (3/4)) (3/4) 
            . timeSignature (2/4)
        
        left = (dynamics pp . legato) 
               (scat [a,g,f,e] |> d^*2)
            |> (dynamics ((mp |> mp `cresc` mf |> mf)^*8) . legato) 
               (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)
        
        right = up' _P4 . delay 2 $ 
               (dynamics pp . legato) 
               (scat [a,g,f,e] |> d^*2)
            |> (dynamics mp . legato) 
               (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)

    in meta $ compress 8 $ left </> down _P8 right



openAudio :: Score Note -> IO ()    
openAudio x = do
    void $ writeMidi "test.mid" $ x
    void $ system "timidity -Ow test.mid"
    void $ system "open -a Audacity test.wav"
