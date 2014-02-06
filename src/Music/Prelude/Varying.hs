
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    DeriveDataTypeable,
    MultiParamTypeClasses, -- TODO
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

module Music.Prelude.Varying (
        module Music.Score,
        module Music.Pitch,
        module Music.Dynamics,
        module Music.Parts,
        Note,
        asScore,
        asVoice,
        asTrack,
        asNote,
        open,
        play,
        openAndPlay
  ) where

import Data.Default
import Data.Typeable
import Control.Applicative -- TODO below
import Control.Lens hiding ((??)) -- TODO below

import Music.Pitch
import Music.Dynamics
import Music.Parts
import Music.Score hiding (Pitch, Interval, Fifths, Note, Part, pitch)
import Music.Time.Behavior -- TODO
import qualified Music.Score -- TODO below

import Music.Prelude.Instances ()


asNote :: Note -> Note
asNote = id

asScore :: Score Note -> Score Note
asScore = id

asVoice :: Voice Note -> Voice Note
asVoice = id

asTrack :: Track Note -> Track Note
asTrack = id

-- newtype BasicPart = BasicPart { getBasicPart :: Integer }
--     deriving (Eq, Ord, Num, Integral, Real, Enum, Typeable)
-- 
-- instance Default BasicPart where def = BasicPart 0
-- instance Show BasicPart where
--     show _ = ""

type Note = (PartT Part
    (TremoloT
      (TextT
        (ArticulationT
          (HarmonicT
            (TieT
              (SlideT
                (DynamicT
                  (ChordT Pitch)))))))))
                  -- (Behavior
                  --   (First
                  --     Pitch))))))))))

open          = openLilypond . asScore
play          = playMidiIO "to Gr" . asScore
openAndPlay x = open x >> play x


{-

-- TODO move

instance Functor First where
    fmap f = (pure f <*>)
instance Applicative First where
    pure = First
    First f <*> First x = First $ f x

type instance Music.Score.Pitch (First a) = Music.Score.Pitch a
instance HasSetPitch a b => HasSetPitch (First a) (First b) where
    type SetPitch g (First a) = First (SetPitch g a)
    mapPitch f = fmap (mapPitch f)

type instance Music.Score.Pitch (Behavior a) = Behavior (Music.Score.Pitch a)
instance HasSetPitch a b => HasSetPitch (Behavior a) (Behavior b) where
    type SetPitch g (Behavior a) = Behavior (SetPitch g a)
    -- mapPitch f = undefined



instance HasLilypond a => HasLilypond (Behavior a) where
    getLilypond d x = getLilypond d (x ?? 0)
instance HasLilypond a => HasLilypond (First a) where
    getLilypond d = getLilypond d . getFirst
instance Tiable a => Tiable (Behavior a) where
    beginTie = fmap beginTie
    endTie = fmap endTie    
instance Tiable a => Tiable (First a) where
    beginTie = fmap beginTie
    endTie = fmap endTie    

instance HasMidi a => HasMidi (Behavior a) where
    getMidi x = getMidi (x ?? 0)
instance HasMidi a => HasMidi (First a) where
    getMidi = getMidi . getFirst

instance IsPitch a => IsPitch (Behavior a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Behavior a) where
    fromInterval = pure . fromInterval
instance IsPitch a => IsPitch (First a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (First a) where
    fromInterval = pure . fromInterval

    
-- instance Semigroup a => Semigroup (Behavior a) where
    -- (<>) = liftA2 (<>)

-- TODO move
pitch_' :: HasSetPitch' a => Setter' a (Music.Score.Pitch a)
pitch_' = sets mapPitch

-}
