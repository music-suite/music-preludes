
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

type Note = (PartT BasicPart
    (TremoloT
      (ChordT      
        (TieT
          (HarmonicT (SlideT
            (DynamicT (ArticulationT (TextT Pitch)))))))))

open = openLy . asScore



-- TODO move
-- TODO rename
pcatLy :: [Lilypond] -> Lilypond
pcatLy = foldr L.pcat (L.Simultaneous False [])

scatLy :: [Lilypond] -> Lilypond
scatLy = foldr L.scat (L.Sequential [])

instance IsPitch a => IsPitch (ChordT a) where
    fromPitch = ChordT . return . fromPitch

instance HasLilypond a => HasLilypond (ChordT a) where
    getLilypond d = pcatLy . fmap (getLilypond d) . getChordT


-----------------------------
-- A propos #40:

-- We want certain transformers to be outside the ChordT transformer (PartT n, TremoloT)
-- This makes sense as chords *must* share these attributes
--
-- Problem with the simultanous function
--
--
-- This can be implemented: it merges simultaneous events
simult :: Score a -> Score (ChordT a)
simult = undefined

-- Also we can do monadic join on chords
joinChord :: Score (ChordT (ChordT a)) -> Score (ChordT a)
joinChord = undefined

-- Hence we also have
bindSimult :: Score (ChordT a) -> Score (ChordT a)
bindSimult = joinChord . simult

-- That is: given a score of chords of notes (with harmonics, dynamics, pitch etc), we
-- can merge all simultaneous chords together so that each chord has a unique era.

-- On the other hand: given a score of chords of notes where the chords have separate properties
-- (part and tremolo), how can we join simultaneous events? This works if the outer properties
-- form a semigroup, i.e. tremolo can use Max.

-- For some attributes (notably parts) there is no way to do this. I.e. given a score of chords
-- in various parts, we can only merge chords that are in the same part.



-- How to lift this to outermost level?
-- (Score a -> Score b) -> Score (PartT n a) -> Score (PartT n b)
-- (Score a -> Score b) -> Score (TieT a) -> Score (TieT b)
-- (Score a -> Score b) -> Score (TremoloT a) -> Score (TremoloT b)




-- TODO These instances should be moved, see music-score #67

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


