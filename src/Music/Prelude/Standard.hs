
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

module Music.Prelude.Standard (
        module Music.Score,
        module Music.Pitch,
        module Music.Dynamics,
        module Music.Parts,
        StandardNote,
        asScore,
        asVoice,
        asTrack,
        asNote,
        open,
        play,
        openAndPlay
  ) where

import           Data.Default
import           Data.Typeable

import           Music.Dynamics
import           Music.Parts
import           Music.Pitch
import           Music.Score             hiding (Fifths, Interval, Note, Part, Pitch)

import           Music.Prelude.Instances ()

asNote :: StandardNote -> StandardNote
asNote = id

asScore :: Score StandardNote -> Score StandardNote
asScore = id

asVoice :: Voice StandardNote -> Voice StandardNote
asVoice = id

asTrack :: Track StandardNote -> Track StandardNote
asTrack = id

-- newtype BasicPart = BasicPart { getBasicPart :: Integer }
--     deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)
--
-- instance Default BasicPart where def = BasicPart 0
-- instance Show BasicPart where
--     show _ = ""

type StandardNote = 
  (PartT Part
    (ColorT 
      (TextT
        (TremoloT
          (HarmonicT
            (SlideT
              (ArticulationT (Average Double, Average Double)
                (DynamicT (Average Double)
                  [TieT
                    (Behavior StandardPitch)]))))))))

type StandardPitch = Music.Pitch.Pitch
-- data StandardPitch = StandardPitch Music.Pitch.Pitch
    -- deriving (HasMidi, HasLilypond, HasMusicXml, HasPitch)


open          = openLilypond . asScore
play          = error "Not implemented: play"
openAndPlay x = open x >> play x

