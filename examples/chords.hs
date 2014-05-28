
{-# LANGUAGE NoMonomorphismRestriction #-}

import Music.Prelude

import Control.Concurrent.Async
import Control.Applicative
import System.Process (system)

kStrum = 0.03

main   = do
  -- openLilypond (music^/4)
  -- openMidi music
  openAudacity music

guitar = (tutti $ StdInstrument 26)
alto   = (tutti $ StdInstrument 65)
rh     = (tutti $ StdInstrument 113)


-- Strum a chord
strum :: [Score a] -> Score a
strum = pcat . zipWith (\t x -> delay t . stretchTo (x^.duration ^-^ t) $ x) [0,kStrum..]

counterRh = set parts' rh $ (mcatMaybes $ times 4 $ octavesUp 1 $ scat [rest^*2,g,g,g^*2,g^*2, rest^*2, scat [g,g,g]^*2])^/8

strings = set parts' (tutti violin) $ octavesAbove 1 $ 
     (c_<>e_<>g_)^*4 
  |> (c_<>fs_<>a_)^*4
  |> (g__<>c_<>e_)^*4 
  |> (c_<>f_<>g_)^*4

melody = octavesDown 1 $ set parts' (tutti horn) $ 
  (scat [c',g'^*2,e',d',c'^*2,b,c'^*2,d'^*2,e',d',c'^*2]^/4)
  |>
  (scat [c',a'^*2,e',d',c'^*2,b,c'^*2,d'^*2,eb',d',c']^/4)
  

music = asScore  $ 
  (<> melody) $
  (<> strings) $
  (<> counterRh) $
  
  set parts' guitar $ 
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,e_,g_,c,e,g])
  |>
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,fs_,a_,c,fs,a])
  |>
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,e_,g_,c,e,g])
  |>
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [g_,a_,c,f,a,c'])
  

openAudacity :: Score Note -> IO ()    
openAudacity x = do
    void $ writeMidi "test.mid" $ x
    void $ system "timidity -Ow test.mid"
    void $ system "open -a Audacity test.wav"
