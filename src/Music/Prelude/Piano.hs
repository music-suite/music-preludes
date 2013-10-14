
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable #-} 

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
-- A basic music representation for piano.
--
-------------------------------------------------------------------------------------

module Music.Prelude.Piano (
        module Music.Score,
        PianoPart,
        Note,
        asScore
  ) where

import Music.Pitch
import Music.Score hiding (Pitch, Interval, Note)
import Music.Dynamics.Literal --TODO
import Data.Typeable
import Data.AffineSpace.Point
import qualified Music.Score as Score
import qualified Music.Lilypond as Lilypond
import qualified Music.MusicXml.Simple as Xml

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
