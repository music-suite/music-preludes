
module Music.Prelude.StringQuartet (
        module Music.Score,
        module Music.Score.Combinators,
        module Music.Pitch.Literal,
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,
        NotePart,
        Note,
        putXml,
        showXml,
        asScore
  ) where

import Music.Score
import Music.Score.Combinators
import Music.Pitch.Literal
import Music.Dynamics.Literal
import Data.Semigroup
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace

import qualified Music.MusicXml as Xml

putXml  = putStrLn . Xml.showXml . toXml . asScore
showXml = Xml.showXml . toXml . asScore

asScore :: Score Note -> Score Note
asScore = id

data NotePart
    = Vl1
    | Vl2
    | Vla
    | Vc
    deriving (Eq, Ord, Enum)

instance Show NotePart where
    show Vl1  = "Violin I"
    show Vl2  = "Violin II"
    show Vla  = "Viola"
    show Vc   = "Cello"

type Note = (PartT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
