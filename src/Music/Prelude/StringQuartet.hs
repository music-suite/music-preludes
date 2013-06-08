
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

module Music.Prelude.StringQuartet (
        module Music.Score,
        module Music.Pitch.Literal,
        module Music.Dynamics.Literal,
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,
        NotePart,
        Note,
        asScore
  ) where

import Music.Score
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace

asScore :: Score Note -> Score Note
asScore = id

data NotePart
    = Vl1
    | Vl2
    | Vla
    | Vc
    deriving (Eq, Ord, Enum, Typeable)

instance Show NotePart where
    show Vl1  = "Violin I"
    show Vl2  = "Violin II"
    show Vla  = "Viola"
    show Vc   = "Cello"

type Note = (PartT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
