
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    FlexibleContexts,
    UndecidableInstances,
    TupleSections,
    ViewPatterns,
    DeriveDataTypeable,
    ConstraintKinds,
    RankNTypes,
    FlexibleInstances, -- For the (Time ->) instances
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
import Control.Lens hiding ((|>)) -- TODO below

import Music.Pitch
import Music.Dynamics.Literal -- TODO
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

-- DEBUG
type P a = Music.Score.Pitch a
type I a = Music.Score.Interval a
type B a = Behavior a
{-
    Test a type constraint like this:
    
        :t () :: (Int ~ Float) => ()

        
        
    For example this fails:

        () :: forall a b . (a ~ b, b ~ Int, a ~ Float) => ()
    
-}
-- type Yup  dummy = forall a b . (a ~ b, dummy ~ dummy)
-- type Nope dummy = forall a b . (a ~ b, b ~ Int, a ~ Float)


type instance Music.Score.Pitch (Time -> a) = Time -> (Music.Score.Pitch a)


type instance Music.Score.Pitch (First a) =  Music.Score.Pitch a

instance HasGetPitch a => HasGetPitch (First a) where
    __getPitch (First a) = __getPitch a

instance (HasSetPitch a b) => HasSetPitch (First a) (First b) where
    type SetPitch p (First a) = First (SetPitch p a)
    __mapPitch f = fmap (spitch %~ f) 

spitch :: HasSetPitch a b => Setter a b (P a) (P b)
spitch = sets __mapPitch

spitch' :: HasSetPitch a a => Setter' a (P a)
spitch' = sets __mapPitch

type Note = (PartT Part
    (TremoloT
      (TextT
        (ArticulationT
          (HarmonicT
            (TieT
              (SlideT
                (DynamicT
                  (Behavior
                    (First
                      Pitch))))))))))

instance IsDynamics a => IsDynamics (First a) where
    fromDynamics = pure . fromDynamics
instance IsDynamics a => IsDynamics (Last a) where
    fromDynamics = pure . fromDynamics



instance Tiable a => Tiable (First a) where toTied x = (x,x)
instance HasLilypond a => HasLilypond (First a) where
    getLilypond d = getLilypond d . getFirst

instance HasMusicXml a => HasMusicXml (First a) where
    getMusicXml d = getMusicXml d . getFirst


instance HasMidi a => HasMidi (First a) where
    getMidi = getMidi . getFirst












instance IsPitch a => IsPitch (Time -> a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Time -> a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (Time -> a) where
    fromDynamics = pure . fromDynamics

instance Tiable a => Tiable (Time -> a) where toTied x = (x,x)
instance HasLilypond a => HasLilypond (Time -> a) where
    getLilypond d = getLilypond d . (? 0)
instance HasMidi a => HasMidi (Time -> a) where
    getMidi = getMidi . (? 0)








open          = openLilypond . asScore
play          = playMidiIO "to Gr" . asScore
openAndPlay x = open x >> play x

-- TODO move
instance AdditiveGroup Semitones where
    zeroV = 0
    negateV = negate
    (^+^) = (+)
instance VectorSpace Semitones where
    type Scalar Semitones = Semitones
    (*^) = (*)
instance IsInterval Semitones where
    fromInterval = semitones . asInterval . fromInterval

instance AdditiveGroup Hertz where
    zeroV = 0
    negateV = negate
    (^+^) = (+)
instance VectorSpace Hertz where
    type Scalar Hertz = Hertz
    (*^) = (*)
instance IsInterval Hertz where
    -- TODO octave
    fromInterval (IntervalL (_,d,c)) = case (d,c) of
        (0,0)  -> 1/1
        (1,2)  -> 9/8 -- or 10/9
        (2,4)  -> 5/4
        (3,5)  -> 4/3
        (4,7)  -> 3/2
        (5,9)  -> 8/5
        (5,11) -> 5/3
        (6,12)  -> 15/8




-- I.e. majorScale = scale [2,2,1,2,2,1]
-- scale :: (Integral b, AdditiveGroup b) => [b] -> b -> b


majorS :: (Integral a, Integral (Scalar v), VectorSpace v, IsInterval v) => a -> v
majorS = scale [_M2,_M2,m2,_M2,_M2,_M2,m2]


minorS :: (Integral a, Integral (Scalar v), VectorSpace v, IsInterval v) => a -> v
minorS = scale [_M2,m2,_M2,_M2,_M2,_M2,m2]


scale :: (Integral a, Integral (Scalar v), VectorSpace v) => [v] -> a -> v
scale [] = error "Invalid scale"
scale s = scaleAbs (scanl (^+^) zeroV s)

scaleAbs :: (Integral a, Integral (Scalar v), VectorSpace v) => [v] -> a -> v
scaleAbs [] x = error "Invalid scale"
scaleAbs s x = inst*^offset ^+^ s !! fromIntegral step
    where
        (inst, step) = fromIntegral x `divMod` fromIntegral count
        count = length s - 1
        offset = last s




pt = stretch 4 $ 
    varying $ 
        \t -> spell modal $ (\x->x :: Semitones) $ scale [1,3,2,2,1,3] $ toSemitones $ (((* 6) $ sin $ realToFrac t*((pi*2)/5))) - 0

score = asScore $ compress 1 $ pcat [p1,p2,p3,p4]
p1 = part .~ (vl2)          $ spitch' %~ (.+^ (stretch 2.1 pt)) $ times 80 (stretchTo 2 $ c' |> (b^*(2/4))^/1)
p2 = part .~ (vl1)          $ spitch' %~ (.+^ (stretch 2.2 pt)) $ times 80 (stretchTo 3 $ c' |> (b^*(2/4))^/1)
p3 = part .~ (tutti viola)  $ spitch' %~ (.+^ (stretch 2.3 pt)) $ times 80 (stretchTo 4 $ c' |> (b^*(2/4))^/1)
p4 = part .~ (tutti cello)  $ spitch' %~ (.+^ (stretch 2.4 pt)) $ times 80 (stretchTo 5 $ c' |> (b^*(2/4))^/1)

[vl1, vl2] = divide 2 (tutti violin)

toSemitones :: RealFrac a => a -> Semitones
toSemitones = floor

showPitches :: [Pitch] -> Score (PartT Integer (ChordT Pitch))
showPitches = scat . fmap (return . PartT . (1,) . ChordT  .return . asPitch)

part :: (HasPart a, Default (Music.Score.Part a)) => Lens' a (Music.Score.Part a)
part = lens getPart (flip setPart)

toNote :: Pitch -> Note
-- toNote p = __setPitch (pure p) c
toNote = pure . pure . pure . pure . pure . pure . pure . pure . pure . pure

-- TODO
instance Monoid Part where
    mempty = def
    mappend = const
instance Applicative DynamicT where
    pure x = DynamicT (False,False,Nothing,x,False,False)
-- instance Applicative SlideT where
--     pure x = SlideT (False,False,x,False,False)
-- instance Applicative TremoloT where
--     pure x = TremoloT (0,x)
-- instance Applicative TextT where
--     pure x = TextT (mempty,x)
-- instance Applicative ArticulationT where
--     pure x = ArticulationT (False,False,0,0,x,False)
-- instance Applicative HarmonicT where
--     pure x = HarmonicT ((False,0), x)
instance Applicative TieT where
    pure x = TieT (False,x,False)      

