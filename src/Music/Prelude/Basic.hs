
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
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
      -- (ChordT      
        (TieT
          (HarmonicT (SlideT
            (DynamicT (ArticulationT (TextT Pitch))))))))
            -- )

open = openLy . asScore


instance IsPitch a => IsPitch (ChordT a) where
    fromPitch = ChordT . return . fromPitch

instance HasMidi a => HasMidi (ChordT a) where
    getMidi = pcat . fmap getMidi . getChordT


-----------------------------
-- A propos #40:

-- We want certain transformers to be outside the ChordT transformer (PartT n, TremoloT)
-- This makes sense as chords *must* share these attributes
--
-- Problem with the simultanous function
--
--
-- This can be implemented: it merges simultaneous events
simult :: Score a -> Score (ChordT a)
simult = undefined

-- Also we can do monadic join on chords
joinChord :: Score (ChordT (ChordT a)) -> Score (ChordT a)
joinChord = undefined

-- Hence we also have
bindSimult :: Score (ChordT a) -> Score (ChordT a)
bindSimult = joinChord . simult

fixSimult :: Score Note -> Score Note
fixSimult = undefined

mapPartWithout :: (Score a -> Score a) -> Score (PartT n a) -> Score (PartT n a)
mapPartWithout = undefined


data P = PW
data A = AW
x :: Score (PartT P A)
x = undefined

-- fmap split x :: Score (P, A)


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

type Note2 = 
  (ChordT      
    (PartT BasicPart
      (TremoloT
        (TieT
          (HarmonicT (SlideT
            (DynamicT (ArticulationT (TextT Pitch)))))))))

-- j :: Note -> Note2
-- j = juggle . fmap juggle


juggle :: (Splittable t, Splittable u) => t (u a) -> u (t a)
juggle = (uncurry fuse) . second' (uncurry fuse) . juggle' . second' split . split


juggle' (a, (b, c)) = (b, (a, c))
second' f (a,b) = (a,f b)

-- juggle :: (Splittable t n, Splittable u n) => n -> u a -> t a
-- juggle _ = uncurry (\x y -> fuse x y) . split 

{-
    instance Splittable t x
    instance Splittable u y

    juggle x =
        uncurry (\x ua -> (x, fuse x ua)) . split x

    t (u a) -> u (t a)
    t (u a)
    (x, u a)
    (x, u (t a))
    u (t a)




    f x (g y a) -> g y (f x a)
    f x (g y a)
    (x, g y a)
    (x, g y (f x a))
    g y (f x a)

-}



-- How to lift this to outermost level?
-- (Score a -> Score b) -> Score (PartT n a) -> Score (PartT n b)
-- (Score a -> Score b) -> Score (TieT a) -> Score (TieT b)
-- (Score a -> Score b) -> Score (TremoloT a) -> Score (TremoloT b)




-- TODO These instances should be moved, see music-score #67

instance HasPitch Pitch where { type PitchOf Pitch = Pitch ; getPitch = id; modifyPitch = id }

instance Tiable Pitch where { beginTie = id ; endTie = id }

instance HasMidi Semitones where
    getMidi a = getMidi $ (60 + fromIntegral a :: Integer)

instance HasMidi Pitch where
    getMidi p = getMidi $ semitones (p .-. c)

instance HasMusicXml Pitch where
    getMusicXml d p = Xml.note (pc, Just acc, oct+4) (frac d)
        where
            pc   = toEnum $ fromEnum $ name p
            acc  = fromIntegral $ accidental p
            oct  = fromIntegral $ octaves (p .-. c)
            frac = fromRational . toRational

instance HasLilypond Pitch where
    getLilypond d p = L.note (L.NotePitch (L.Pitch (pc,acc,oct+5)) Nothing) ^*(frac d*4)
        where
            pc   = toEnum $ fromEnum $ name p
            acc  = fromIntegral $ accidental p
            oct  = fromIntegral $ octaves (p .-. c)
            frac = fromRational . toRational


