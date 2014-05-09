
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- TODO debug
{-# LANGUAGE TypeFamilies #-} -- TODO debug

import Music.Prelude.Basic
import System.Process (system)

{-  
  Bela Bartok: Wandering (excerpt)
  From Mikrokosmos, vol. III

  Inspired by the Abjad transcription
-}

main = openLilypond music



music :: Score BasicNote
music = let
    meta = id
      . title "Mikrokosmos (excerpt)"
      . composer "Bela Bartok"
      . timeSignature (2/4)
      . timeSignatureDuring ((2/4) >-> (5/4)) (3/4) 
    
    left = (level pp {-. legato-}) 
         (scat [a,g,f,e] |> d^*2)
      |> {-(level ((mp |> mp `cresc` mf |> mf)^*8) . legato)-}id 
         (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)
    -- 
    right = up _P4 . delay 2 $ 
         (level pp {-. legato-}) 
         (scat [a,g,f,e] |> d^*2)
      |> (level mp {-. legato-}) 
         (scat [g,f,e,d] |> c |> (d |> e)^/2 |> f |> e |> d^*8)

  in meta $ compress 8 $ left </> down _P8 right


-- openAudio :: Score Note -> IO ()  
openAudio x = do
  void $ writeMidi "test.mid" $ x
  void $ system "timidity -Ow test.mid"
  void $ system "open -a Audacity test.wav"


cresc = const