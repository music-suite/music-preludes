
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

module Music.Prelude.Piano (
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
    = Pno
    deriving (Eq, Ord, Enum)

instance Show NotePart where
    show Pno  = "Piano"

type Note = (PartT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
