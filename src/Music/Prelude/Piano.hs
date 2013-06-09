
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

module Music.Prelude.Piano (
        module Music.Score,
        PianoPart,
        Note,
        asScore
  ) where

import Music.Score
import Data.Typeable

asScore :: Score Note -> Score Note
asScore = id

data PianoPart
    = Pno
    deriving (Eq, Ord, Enum)

instance Show PianoPart where
    show Pno  = "Piano"

type Note = (PartT PianoPart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
