
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
        asBasicNote,
        open,
        play,
        openAndPlay
  ) where

import           Data.Default
import           Data.Typeable

import           Music.Dynamics
import           Music.Parts             hiding (Part)
import           Music.Pitch             hiding (Fifths, Interval, Note, Part,
                                          Pitch, pitch)
import qualified Music.Pitch             as Pitch
import           Music.Score

import           Music.Prelude.Instances ()

asBasicNote :: BasicNote -> BasicNote
asBasicNote = id

asScore :: Score BasicNote -> Score BasicNote
asScore = id

asVoice :: Voice BasicNote -> Voice BasicNote
asVoice = id

asTrack :: Track BasicNote -> Track BasicNote
asTrack = id

type BasicNote = (PartT BasicPart
    (ArticulationT
      (TieT
        (DynamicT
          (ChordT
            Pitch.Pitch)))))

open          = openLilypond . asScore
play          = playMidiIO mempty . asScore
openAndPlay x = open x >> play x

