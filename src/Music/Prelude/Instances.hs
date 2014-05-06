
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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

module Music.Prelude.Instances () where

import           Data.AffineSpace.Point
import           Data.Default
import           Data.Typeable

import           Music.Dynamics
import           Music.Parts
import           Music.Pitch
import           Music.Score            hiding (Fifths, Interval, Note, Pitch)

import qualified Music.Lilypond         as Lilypond
import qualified Music.MusicXml.Simple  as Xml
import qualified Music.Score            as Score

deriving instance Typeable Music.Parts.Part

instance Transformable Music.Parts.Part where
  transform _ = id
type instance Music.Score.Part Music.Parts.Part = Music.Parts.Part
type instance SetPart a Music.Parts.Part = a

instance (Transformable a, a ~ Music.Score.Part a) => HasPart Music.Parts.Part a where
  part = ($)
instance (Transformable a, a ~ Music.Score.Part a) => HasParts Music.Parts.Part a where
  parts = ($)



instance Transformable BasicPart where
  transform _ = id
type instance Music.Score.Part BasicPart = BasicPart
type instance SetPart a BasicPart = a

instance (Transformable a, a ~ Music.Score.Part a) => HasPart BasicPart a where
  part = ($)
instance (Transformable a, a ~ Music.Score.Part a) => HasParts BasicPart a where
  parts = ($)

instance Transformable Pitch where
  transform _ = id
type instance Score.Pitch Pitch = Pitch
type instance SetPitch a Pitch = a

instance (Transformable a, a ~ Score.Pitch a) => HasPitch Pitch a where
  pitch = ($)
instance (Transformable a, a ~ Score.Pitch a) => HasPitches Pitch a where
  pitches = ($)

instance Tiable Pitch where
    beginTie = id
    endTie = id

instance HasMidi Semitones where
    getMidi a = getMidi $ (fromIntegral a :: Integer)

instance HasMidi Pitch where
    getMidi p = getMidi $ semitones (p .-. c)

instance HasMusicXml Pitch where
    getMusicXml      (realToFrac -> d) = (`Xml.note` d) . snd3 Just . spellPitch 4
    getMusicXmlChord (realToFrac -> d) = (`Xml.chord` (realToFrac d)) . fmap (snd3 Just . spellPitch 4)

instance HasLilypond Pitch where
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note . pitchLilypond . Lilypond.Pitch . spellPitch 5
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLilypond . Lilypond.Pitch . spellPitch 5)

-- TODO move
snd3 f (a, b, c) = (a, f b, c)
pitchLilypond a = Lilypond.NotePitch a Nothing

spellPitch :: (Enum p, Num a, Num o) => Octaves -> Pitch -> (p, a, o)
spellPitch referenceOctave p = (pitchName, pitchAccidental, octave)
    where
        pitchName       = toEnum $ fromEnum $ name p
        pitchAccidental = fromIntegral $ accidental p
        octave          = fromIntegral $ (+ referenceOctave) $ octaves (p .-. c)

instance HasMidiProgram BasicPart where
    getMidiChannel _ = 0
    getMidiProgram _ = 0

instance HasMidiProgram Music.Parts.Part where
    getMidiChannel = defaultMidiChannel
    getMidiProgram = fixStrings . defaultMidiProgram
        where
            fixStrings x = case x of
                40 -> 48
                41 -> 48
                42 -> 48
                x  -> x
