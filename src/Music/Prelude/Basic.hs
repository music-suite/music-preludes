
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    MultiParamTypeClasses,
    DeriveDataTypeable,
    TypeFamilies #-} 

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
        module Music.Dynamics.Literal,
        BasicPart,
        Note,
        asScore
  ) where

import Music.Pitch
import Music.Score hiding (Pitch, Interval, Fifths, Note)
import Music.Dynamics.Literal --TODO
import Data.Default
import Data.Typeable
import Data.AffineSpace.Point
import qualified Music.Score as Score
import qualified Music.Lilypond as Lilypond
import qualified Music.MusicXml.Simple as Xml

asScore :: Score Note -> Score Note
asScore = id

asVoice :: Voice Note -> Voice Note
asVoice = id

asTrack :: Track Note -> Track Note
asTrack = id

newtype BasicPart = BasicPart { getBasicPart :: Integer }
    deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)
instance HasPart BasicPart where type Part BasicPart = BasicPart; getPart = id ; modifyPart = id
instance Default BasicPart where def = BasicPart 0
instance Show BasicPart where
    show (BasicPart x)  = "Voice " ++ show x

type Note = (PartT BasicPart
    (TremoloT
      (TextT 
        (ArticulationT 
          (HarmonicT 
            (TieT
              (SlideT
                (DynamicT 
                  (ChordT      
                    Pitch)))))))))

type Note3 =
                  (ChordT      
                    Pitch)

open          = openLy . asScore
play          = playMidiIO "to Gr" . asScore
openAndPlay x = open x >> play x




-- TODO These instances should be moved, see music-score #67

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
    getMusicXml      d = (`Xml.note` (realToFrac d)) . third' Just . spellPitch
    getMusicXmlChord d = (`Xml.chord` (realToFrac d)) . fmap (third' Just . spellPitch)

instance HasLilypond Pitch where
    getLilypond      d = (^*realToFrac (d*4)) . Lilypond.note . pitchLy . Lilypond.Pitch . spellPitch . (.+^ perfect octave)
    getLilypondChord d = (^*realToFrac (d*4)) . Lilypond.chord . fmap (pitchLy . Lilypond.Pitch . spellPitch . (.+^ perfect octave))

-- TODO move
third' f (a, b, c) = (a, f b, c)
pitchLy a = Lilypond.NotePitch a Nothing
spellPitch p = (pc, acc, oct)
    where
        pc   = toEnum $ fromEnum $ name p
        acc  = fromIntegral $ accidental p
        oct  = fromIntegral $ (+ 4) $ octaves (p .-. c)

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




