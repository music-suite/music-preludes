
{-# LANGUAGE OverloadedStrings #-}

import Music.Prelude

-- A simple subject
subj  = times 20 $ scat [c,d,e,f]^/8 |> scat [g,fs]^/2


-- The 
music = id
  $ title "Dynamics"
  $ composer "Anonymous"
  $ (level (stretch 7.9 sine*fff) $ subj) </> (level (stretch 5.5 sine*fff) $ subj)

main  = open $ asScore $ music
