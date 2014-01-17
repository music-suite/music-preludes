
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable,
    TypeFamilies #-}

------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- A basic music representation.
--
-------------------------------------------------------------------------------------

module Music.Prelude.Basic (
        module Music.Score,
        module Music.Pitch,
        module Music.Dynamics,
        module Music.Parts,
        Note,
        asScore,
        asVoice,
        asTrack,
        asNote,
        open,
        play,
        openAndPlay
  ) where

import Data.Default
import Data.Typeable

import Music.Pitch
import Music.Dynamics
import Music.Parts
import Music.Score hiding (Pitch, Interval, Fifths, Note, Part, pitch)

import Music.Prelude.Instances ()

asNote :: Note -> Note
asNote = id

asScore :: Score Note -> Score Note
asScore = id

asVoice :: Voice Note -> Voice Note
asVoice = id

asTrack :: Track Note -> Track Note
asTrack = id

type Note = (PartT BasicPart
    (TremoloT
      (TextT
        (ArticulationT
          (HarmonicT
            (TieT
              (SlideT
                (DynamicT
                  (ChordT
                    Pitch)))))))))

open          = openLy . asScore
play          = playMidiIO "to Gr" . asScore
openAndPlay x = open x >> play x

