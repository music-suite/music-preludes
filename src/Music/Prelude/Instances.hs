
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    MultiParamTypeClasses,
    DeriveDataTypeable,
    TypeFamilies, 
    FlexibleInstances,
    UndecidableInstances,
    ViewPatterns #-} 

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

import Data.Default
import Data.Typeable
import Data.AffineSpace.Point

import Music.Pitch
import Music.Dynamics
import Music.Parts
import Music.Score hiding (Pitch, Interval, Fifths, Note)

import qualified Music.Score as Score
import qualified Music.Lilypond as Lilypond
import qualified Music.MusicXml.Simple as Xml

deriving instance Typeable Music.Parts.Part

type instance Music.Score.Part BasicPart = BasicPart
instance HasPart BasicPart where
    getPart = id
    modifyPart = id

-- FIXME
instance Delayable Pitch      
instance Stretchable Pitch
type instance Score.Pitch Pitch = Pitch

instance HasGetPitch Pitch where
    __getPitch = id
instance (a ~ Score.Pitch a) => HasSetPitch Pitch a where
    type SetPitch a Pitch = a
    __mapPitch = id
instance Tiable Pitch where
    beginTie = id
    endTie = id

instance HasMidi Semitones where
    getMidi a = getMidi $ (60 + fromIntegral a :: Integer)

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
