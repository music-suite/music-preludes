
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
        Note,
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
import           Music.Score             hiding (Fifths, Interval, Note, Part,
                                          Pitch, pitch)

import           Music.Prelude.Instances ()

asNote :: Note -> Note
asNote = id

asScore :: Score Note -> Score Note
asScore = id

asVoice :: Voice Note -> Voice Note
asVoice = id

asTrack :: Track Note -> Track Note
asTrack = id

-- newtype BasicPart = BasicPart { getBasicPart :: Integer }
--     deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)
--
-- instance Default BasicPart where def = BasicPart 0
-- instance Show BasicPart where
--     show _ = ""

type Note = 
  (PartT Part
    (TieT
      (ColorT 
        (TextT
          (TremoloT
            (HarmonicT
              (SlideT
                (ArticulationT (Sum Double, Sum Double)
                  (DynamicT (Sum Double)
                    [Behavior StandardPitch])))))))))

type StandardPitch = Music.Pitch.Pitch
-- data StandardPitch = StandardPitch Music.Pitch.Pitch
    -- deriving (HasMidi, HasLilypond, HasMusicXml, HasPitch)


open          = openLilypond . asScore
play          = error "Not implemented: play"
openAndPlay x = open x >> play x

