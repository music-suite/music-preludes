
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
        module Music.Pitch,
        module Music.Dynamics,
        module Music.Articulation,
        module Music.Parts,
        module Music.Score,
        module Control.Monad.Plus,
        module Control.Lens.Operators,
        BasicNote,
        BasicPitch,
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

import           Music.Pitch             hiding (Fifths, Note, Part)
import qualified Music.Pitch
import           Music.Dynamics
import           Music.Articulation
import           Music.Parts             hiding (Part)
import           Music.Score             hiding (Pitch, Interval)

import           Control.Lens.Operators  hiding ((<.>), (<|), (|>))
import           Control.Monad.Plus

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
  (TextT
    (TieT
      (SlideT
        (TremoloT
          (HarmonicT
            (ArticulationT (Average Double, Average Double)
              (DynamicT (Average Double)
                [Behavior BasicPitch]))))))))

type BasicPitch = Music.Pitch.Pitch

open          = openLilypond . asScore
play          = error "Not implemented: play"
openAndPlay x = open x >> play x


