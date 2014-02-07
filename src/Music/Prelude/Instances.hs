
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

type instance Music.Score.Part BasicPart = BasicPart
instance HasPart BasicPart where
        getPart = id
        modifyPart = id

-- FIXME
instance Delayable Pitch      
instance Stretchable Pitch
type instance Score.Pitch Pitch = Pitch

instance HasGetPitch Pitch where
    getPitch = id
instance (a ~ Score.Pitch a) => HasSetPitch Pitch a where
    type SetPitch a Pitch = a
    mapPitch = id
instance Tiable Pitch where
    beginTie = id
    endTie = id

instance HasMidi Semitones where
    getMidi a = getMidi $ (60 + fromIntegral a :: Integer)

instance HasMidi Pitch where
    getMidi p = getMidi $ semitones (p .-. c)

instance HasMusicXml Pitch where
    getMusicXml      (realToFrac -> d) = (`Xml.note` d) . snd3 Just . spellPitch
    getMusicXmlChord (realToFrac -> d) = (`Xml.chord` (realToFrac d)) . fmap (snd3 Just . spellPitch)

instance HasLilypond Pitch where
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note . pitchLilypond . Lilypond.Pitch . spellPitch . (.+^ perfect octave)
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLilypond . Lilypond.Pitch . spellPitch . (.+^ perfect octave))

-- TODO move
snd3 f (a, b, c) = (a, f b, c)
pitchLilypond a = Lilypond.NotePitch a Nothing

spellPitch :: (Enum p, Num a, Num o) => Pitch -> (p, a, o)
spellPitch p = (pitchName, pitchAccidental, octave)
    where
        pitchName       = toEnum $ fromEnum $ name p
        pitchAccidental = fromIntegral $ accidental p
        octave          = fromIntegral $ (+ 4) $ octaves (p .-. c)

instance Alterable a => Alterable (Score a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (ChordT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (DynamicT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (SlideT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (TieT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (HarmonicT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (ArticulationT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (TextT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (TremoloT a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Alterable a => Alterable (PartT n a) where
    sharpen = fmap sharpen
    flatten = fmap flatten

instance Augmentable a => Augmentable (Score a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (ChordT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (DynamicT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (SlideT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TieT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (HarmonicT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (ArticulationT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TextT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (TremoloT a) where
    augment = fmap augment
    diminish = fmap diminish

instance Augmentable a => Augmentable (PartT n a) where
    augment = fmap augment
    diminish = fmap diminish

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
