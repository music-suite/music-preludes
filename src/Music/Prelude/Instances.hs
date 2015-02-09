
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
-- Provides miscellaneous instances.
--
-------------------------------------------------------------------------------------

module Music.Prelude.Instances () where

import           Data.AffineSpace.Point
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

instance Transformable Hertz where
  transform _ = id
type instance Score.Pitch Hertz = Hertz
type instance SetPitch a Hertz = a

instance (Transformable a, a ~ Score.Pitch a) => HasPitch Hertz a where
  pitch = ($)
instance (Transformable a, a ~ Score.Pitch a) => HasPitches Hertz a where
  pitches = ($)

instance Tiable Pitch where
    beginTie = id
    endTie = id

instance HasBackendNote Midi Semitones where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote Midi Pitch where
  exportNote b = exportNote b . fmap (\p -> semitones (p .-. c))
  exportChord b = exportChord b . fmap (fmap (\p -> semitones (p .-. c)))

instance HasBackendNote SuperCollider Semitones where
  exportNote b = exportNote b . fmap toInteger
  exportChord b = exportChord b . fmap (fmap toInteger)

instance HasBackendNote SuperCollider Pitch where
  exportNote b = exportNote b . fmap (\p -> semitones (p .-. c))
  exportChord b = exportChord b . fmap (fmap (\p -> semitones (p .-. c)))


instance HasBackendNote MusicXml Pitch where
  exportNote  _ (XmlContext d Nothing)    = Xml.rest (realToFrac d)
  exportNote  _ (XmlContext d (Just x))   = (`Xml.note` realToFrac d) . snd3 Just . spellPitch 4 $ x

  exportChord _ (XmlContext d Nothing)    = Xml.rest (realToFrac d)
  exportChord _ (XmlContext d (Just xs))  = (`Xml.chord` (realToFrac d)) . fmap (snd3 Just . spellPitch 4) $ xs

instance HasBackendNote Lilypond Pitch where
  exportNote  _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportNote  _ (LyContext d (Just x))   = (^*realToFrac (d*4)) . Lilypond.note . pitchLilypond . Lilypond.Pitch . spellPitch 5 $ x

  exportChord _ (LyContext d Nothing)    = (^*realToFrac (4*d)) Lilypond.rest
  exportChord _ (LyContext d (Just xs))  = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLilypond . Lilypond.Pitch . spellPitch 5) $ xs

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

instance HasLilypondInstrument BasicPart where
    getLilypondClef = 0

instance HasLilypondInstrument Music.Parts.Part where
    getLilypondClef = defaultClef

instance HasMusicXmlInstrument BasicPart where
    getMusicXmlClef = 0
    getMusicXmlNumberOfStaves = 1

instance HasMusicXmlInstrument Music.Parts.Part where
    getMusicXmlClef = defaultClef
    getMusicXmlNumberOfStaves p
      | p == harp                 = 2
      | p^._instrument == piano   = 2
      | p^._instrument == celesta = 2
      | otherwise                 = 1

