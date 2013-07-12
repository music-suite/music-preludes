
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

module Music.Prelude.Basic (
        module Music.Score,
        BasicPart,
        Note,
        asScore
  ) where

import Music.Score
import Data.Typeable

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
        (DynamicT (ArticulationT (TextT Integer))))))))
