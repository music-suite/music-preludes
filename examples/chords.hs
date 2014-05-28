
{-# NoMonomorphismRestriction#-}

import Music.Prelude

kStrum = 0.02

main   = {-openLilypond music >>-} openMidi music
guitar = (tutti $ StdInstrument 26)
alto   = (tutti $ StdInstrument 65)


-- Strum a chord
strum :: [Score a] -> Score a
strum = pcat . zipWith (\t x -> delay t . stretchTo (x^.duration ^-^ t) $ x) [0,kStrum..]

strings = set parts' (tutti violin) $ octavesAbove 1 $ (c_<>e_<>g_)^*4 |> (c_<>fs_<>a_)^*4

melody = octavesDown 1 $ set parts' (tutti horn) $ 
  (scat [c',g'^*2,e',d',c'^*2,b,c'^*2,d'^*2,e',d',c'^*2]^/4)
  |>
  (scat [c',a'^*2,e',d',c'^*2,b,c'^*2,d'^*2,eb',d',c']^/4)
  

music = asScore  $ 
  (<> melody) $
  (<> strings) $
  
  set parts' guitar $ 
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,e_,g_,c,e,g])
  |>
  (pcat $ take 4 $ zipWith delay [0,1..10] $ repeat $ strum [c_,fs_,a_,c,fs,a])
  