
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    MultiParamTypeClasses,
    DeriveDataTypeable,
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
module Music.Prelude.Basic (
        module Music.Score,
        module Music.Pitch,
        module Music.Dynamics.Literal,
        BasicPart,
        Note,
        asScore
  ) where

import Music.Score
import Music.Pitch
import Music.Dynamics.Literal --TODO
import Data.Typeable
import Data.AffineSpace.Point
import qualified Music.Lilypond as L
import qualified Music.MusicXml.Simple as Xml

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

type Note = (PartT BasicPart
    (TremoloT
      (TextT 
        (ArticulationT 
          (HarmonicT 
            (TieT
              (SlideT
                (DynamicT 
                  (ChordT      
                    Pitch)))))))))

type Note3 =
                  (ChordT      
                    Pitch)
open = openLy . asScore


-----------------------------
-- A propos #40:

-- We want certain transformers to be outside the ChordT transformer (PartT n, TremoloT)
-- This makes sense as chords *must* share these attributes
--
-- Problem with the simultanous function
--
--
-- This is implemented: it merges simultaneous events
-- simultaneous :: Score a -> Score (ChordT a)

-- Also we can do monadic join on chords
-- joinChord :: Score (ChordT (ChordT a)) -> Score (ChordT a)
-- joinChord = fmap join
-- 
-- -- Hence we also have
-- bindSimult :: Score (ChordT a) -> Score (ChordT a)
-- bindSimult = joinChord . simultaneous
-- 
-- fixSimult :: Score Note3 -> Score Note3
-- fixSimult = bindSimult
-- 
-- mapPartWithout :: (Score a -> Score a) -> Score (PartT n a) -> Score (PartT n a)
-- mapPartWithout = undefined

{-
That is: given a score of chords of notes (with harmonics, dynamics, pitch etc), we
can merge all simultaneous chords together so that each chord has a unique era.

On the other hand: given a score of chords of notes where the chords have separate properties
(part and tremolo), how can we join simultaneous events? This works if the outer properties
form a semigroup, i.e. tremolo can use Max.

For some attributes (notably parts) there is no way to do this. I.e. given a score of chords
in various parts, we can only merge chords that are in the same part.

What we *really* need is a *partial* semigroup, i.e. a function (<?> :: a -> a -> Maybe a)
Or should we just use mapParts?

====

How can we move around the contenst of a note transformer? I.e. is there a generic way
to go from, say (TremoloT (ArticulationT a)) to (ArticulationT (TremoloT a)). We can
do this if we have a way to decompose and recompose the tyransformers, i.e. functions
like:
    
    split       :: TremoloT a -> (Tremolo, a)
    fuse        :: Tremolo -> a -> TremoloT a
    split       :: ArticulationT a -> (Articulation, a)
    fuse        :: Articulation -> a -> ArticulationT a

so generalize:
-}

{-
class Splittable t where
    type Head t
    split :: t a -> (Head t, a)
    fuse  :: Head t -> a -> t a
    split = undefined
    fuse = undefined

    -- juggle :: (Splittable t ?, Splittable u ?) => t (u a) -> u (t a)


data Articulation = A_

instance Splittable ((,) a) where
    type Head ((,) a) = a
    split = id
    fuse = (,)  
    
instance Splittable ((,,) a b) where
    type Head ((,,) a b) = (a, b)
    split (a,b,c) = ((a,b), c)
    fuse (a,b) c = (a,b,c)
instance Splittable ArticulationT where
    type Head ArticulationT = Articulation
instance Splittable TremoloT where
    type Head TremoloT = Int
instance Splittable (PartT n) where
    type Head (PartT n) = n
instance Splittable ChordT where
    type Head ChordT = ()

juggle :: (Splittable t, Splittable u) => t (u a) -> u (t a)
juggle = (uncurry fuse) . second' (uncurry fuse) . juggle' . second' split . split


juggle' (a, (b, c)) = (b, (a, c))
second' f (a,b) = (a,f b)

-}





-- TODO These instances should be moved, see music-score #67

instance HasPitch Pitch where { type PitchOf Pitch = Pitch ; getPitch = id; modifyPitch = id }

instance Tiable Pitch where { beginTie = id ; endTie = id }

instance HasMidi Semitones where
    getMidi a = getMidi $ (60 + fromIntegral a :: Integer)

instance HasMidi Pitch where
    getMidi p = getMidi $ semitones (p .-. c)

instance HasMusicXml Pitch where
    getMusicXml      d = (`Xml.note` (realToFrac d)) . third' Just . spellPitch
    getMusicXmlChord d = (`Xml.chord` (realToFrac d)) . fmap (third' Just . spellPitch)

instance HasLilypond Pitch where
    getLilypond      d = (^*realToFrac (d*4)) . L.note . pitchLy . L.Pitch . spellPitch . (.+^ perfect octave)
    getLilypondChord d = (^*realToFrac (d*4)) . L.chord . fmap (pitchLy . L.Pitch . spellPitch . (.+^ perfect octave))


spellPitch p = (pc, acc, oct)
    where
        pc   = toEnum $ fromEnum $ name p
        acc  = fromIntegral $ accidental p
        oct  = fromIntegral $ (+ 4) $ octaves (p .-. c)
-- TODO move
pitchLy a = L.NotePitch a Nothing
third' f (a, b, c) = (a, f b, c)


