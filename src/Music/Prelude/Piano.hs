
module Music.Prelude.Piano (
        module Music.Score,
        module Music.Score.Combinators,
        module Music.Pitch.Literal,
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,
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
    = Pno
    deriving (Eq, Ord, Enum)

instance Show NotePart where
    show Pno  = "Piano"

type Note = (VoiceT NotePart (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))
