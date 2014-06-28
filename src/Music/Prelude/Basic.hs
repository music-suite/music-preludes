
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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

import           Music.Dynamics
import           Music.Parts             hiding (Part)
import           Music.Pitch             hiding (Fifths, Note, Part,
                                          pitch)
-- Need to export Pitch.Pitch for transf for now

import qualified Music.Pitch
import           Music.Score             hiding (Pitch, Interval)

import           Control.Lens.Operators  hiding ((<.>), (<|), (|>))
import           Control.Monad.Plus
import Data.Semigroup (Product)

import           Music.Prelude.Instances ()

asBasicNote :: BasicNote -> BasicNote
asBasicNote = id

asScore :: Score BasicNote -> Score BasicNote
asScore = id

asVoice :: Voice BasicNote -> Voice BasicNote
asVoice = id

asTrack :: Track BasicNote -> Track BasicNote
asTrack = id

type Co2 f g a       = f (g a)
type Co3 f g h a     = f (Co2 g h a)
type Co4 f g h i a   = f (Co3 g h i a)
type Co5 f g h i j a = f (Co4 g h i j a)
foo = undefined

type BasicNote          = BasicNote' BasicPart (Sum Double, Sum Double) (Sum Double) [Behavior BasicPitch]
type BasicNote' r a d p  = Co3 (PartT r) ExtraNote (SimpleNote a d) p

newtype ExtraNote a      = ExtraNote { getExtraNote :: Co5 TextT TieT SlideT TremoloT HarmonicT a }
newtype SimpleNote a d p = SimpleNote { getSimpleNote :: Co2 (ArticulationT a) (DynamicT d) p }

type BasicPitch = Music.Pitch.Pitch

[open, play, openAndPlay] = undefined 
-- open          = openLilypond . asScore
-- play          = error "Not implemented: play"
-- openAndPlay x = open x >> play x


