
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

module Music.Prelude.StringQuartet (
        module Music.Score,
        StringQuartetPart,
        Note,
        asScore
  ) where

import Music.Score
import Data.Typeable

asScore :: Score Note -> Score Note
asScore = id

data StringQuartetPart
    = Vl1
    | Vl2
    | Vla
    | Vc
    deriving (Eq, Ord, Enum, Typeable)

instance Show StringQuartetPart where
    show Vl1  = "Violin I"
    show Vl2  = "Violin II"
    show Vla  = "Viola"
    show Vc   = "Cello"

type Note = (PartT StringQuartetPart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
