
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude

-- A simple subject
subj  = times 2 $ scat [c,c,d,b_,e,e]^/16

-- Each voice differ slighly in onset etc
voca v = delay ((4/8)^*v) $ mcatMaybes $ scat $ fmap (\i -> (id $ up (_M2^*i) subj) |> rest^*(15/8)) $ [1..6]

-- The 
music = id
  $ title "Phrases"
  $ timeSignature (3/8) 
  $ over (phrases.middleV) (octavesAbove 1) 
  $ over phrases fuse 
  $ rcat $ map voca [0..3]

main  = open $ asScore $ music
