
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Main where

import Music.Prelude.Basic

-- TODO move
{-
  -o Output file
  -r -f --read --from [format]
  -w -t --write --to [format]
  -p --prelude (default: standard)
  -v --version
  -h --help
  
  Formats:
    midi
    xml musicxml
    ly lilypond
    abc
    guido
    vextab
    sibelius
    pdf
    png
    svg

  Preludes:
    basic
    standard
    
-}
defaultMain = openMusicXml . asScore


main = defaultMain _score

_score = 
  times 10 (scat [c,d]^/6)
