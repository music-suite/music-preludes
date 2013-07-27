
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
        BasicPart,
        Note,
        asScore
  ) where

import Music.Score
import Music.Pitch hiding (Pitch)
import Data.Typeable
import qualified Music.Pitch as P
import qualified Music.Lilypond as L

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
        (DynamicT (ArticulationT (TextT {-Integer-}P.Pitch))))))))


instance Enum a => Enum (Score a) where
    toEnum = return . toEnum
    fromEnum = fromEnum . head . events

instance Enum P.Pitch where
    toEnum = (c .+^) . perfect . fromIntegral
    fromEnum = fromIntegral . number . (.-. c)

instance HasPitch P.Pitch where { type Pitch P.Pitch = P.Pitch ; getPitch = id; modifyPitch = id }
instance Tiable P.Pitch where { beginTie = id ; endTie = id }
instance HasLilypond P.Pitch where
    getLilypond d p = L.note (L.NotePitch (L.Pitch (pc,acc,oct+5)) Nothing) ^*((fromRational . toRational) d*4)
        where
            pc  = toEnum $ fromEnum $ name p
            acc = fromIntegral $ accidental p
            oct = fromIntegral $ octaves (p .-. c)

