
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
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

import Music.Score
import Music.Pitch
import Music.Dynamics.Literal --TODO
import Data.Typeable
import Data.AffineSpace.Point
import qualified Music.Lilypond as L
import qualified Music.MusicXml.Simple as Xml

asScore :: Score Note -> Score Note
asScore = id

asVoice :: Voice Note -> Voice Note
asVoice = id

asTrack :: Track Note -> Track Note
asTrack = id

newtype BasicPart = BasicPart { getBasicPart :: Integer }
    deriving (Eq, Ord, Enum, Typeable)

instance Show BasicPart where
    show _  = ""

type Note = (PartT BasicPart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT {-Integer-}Pitch))))))))

open = openLy . asScore



-- TODO These instances should be moves, see music-score #67

instance HasPitch Pitch where { type PitchOf Pitch = Pitch ; getPitch = id; modifyPitch = id }

instance Tiable Pitch where { beginTie = id ; endTie = id }

instance HasMidi Semitones where
    getMidi a = getMidi $ (60 + fromIntegral a :: Integer)

instance HasMidi Pitch where
    getMidi p = getMidi $ semitones (p .-. c)

instance HasMusicXml Pitch where
    getMusicXml d p = Xml.note (pc, Just acc, oct) (frac d)
        where
            pc   = toEnum $ fromEnum $ name p
            acc  = fromIntegral $ accidental p
            oct  = fromIntegral $ octaves (p .-. c)
            frac = fromRational . toRational

instance HasLilypond Pitch where
    getLilypond d p = L.note (L.NotePitch (L.Pitch (pc,acc,oct+5)) Nothing) ^*(frac d*4)
        where
            pc   = toEnum $ fromEnum $ name p
            acc  = fromIntegral $ accidental p
            oct  = fromIntegral $ octaves (p .-. c)
            frac = fromRational . toRational


