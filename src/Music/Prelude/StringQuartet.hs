
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- A basic music representation for string quartet.
--
-------------------------------------------------------------------------------------
module Music.Prelude.StringQuartet (
        module Music.Score,
        StringQuartetPart,
        Note,
        asScore
  ) where

import           Data.AffineSpace.Point
import           Data.Typeable
import           Music.Dynamics.Literal
import qualified Music.Lilypond         as Lilypond
import qualified Music.MusicXml.Simple  as Xml
import           Music.Pitch
import           Music.Score            hiding (Interval, Note, Pitch)
import qualified Music.Score            as Score

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
