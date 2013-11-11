
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    MultiParamTypeClasses,
    DeriveDataTypeable,
    TypeFamilies, 
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

import Music.Pitch
import Music.Score hiding (Pitch, Interval, Fifths, Note)
import Music.Dynamics.Literal --TODO
import Data.Default
import Data.Typeable
import Data.AffineSpace.Point
import qualified Music.Score as Score
import qualified Music.Lilypond as Lilypond
import qualified Music.MusicXml.Simple as Xml

instance HasPitch Pitch where
        type Pitch Pitch = Pitch
        getPitches  = return
        modifyPitch = id

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
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note . pitchLy . Lilypond.Pitch . spellPitch . (.+^ perfect octave)
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLy . Lilypond.Pitch . spellPitch . (.+^ perfect octave))

-- TODO move
snd3 f (a, b, c) = (a, f b, c)
pitchLy a = Lilypond.NotePitch a Nothing

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

