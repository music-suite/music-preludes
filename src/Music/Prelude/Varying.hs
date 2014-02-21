
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    UndecidableInstances,
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


-- TODO debug
-- type instance Music.Score.Pitch (Score a) = Music.Score.Pitch a
-- instance (HasSetPitch a b, 
--             Transformable (Music.Score.Pitch a), 
--             Transformable (Music.Score.Pitch b)) => 
--                 HasSetPitch (Score a) (Score b) where
--     type SetPitch g (Score a) = Score (SetPitch g a)
--     __mapPitch f  = mapWithSpan (\s -> __mapPitch $ sunder s f)
--         where
--             -- TODO is this generally wrong?
--             sunder s f = sappInv s . f . sapp s
--             sapp    (view delta -> (t,d)) = delayTime t . stretch d
--             sappInv (view delta -> (t,d)) = stretch (recip d) . delayTime (mirror t)
--             -- stretch delay f delay stretch


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

type instance Music.Score.Pitch (Time -> a) = Time -> (Music.Score.Pitch a)


type instance Music.Score.Pitch (Behavior a) = Behavior (Music.Score.Pitch a)
type instance Music.Score.Pitch (First a) =  Music.Score.Pitch a

instance HasGetPitch a => HasGetPitch (First a) where
    __getPitch (First a) = __getPitch a

instance AdditiveGroup a => AdditiveGroup (Behavior a) where
    zeroV = pure zeroV
    negateV = fmap negateV
    (^+^) = liftA2 (^+^)
instance AffineSpace a => AffineSpace (Behavior a) where
    type Diff (Behavior a) = Behavior (Diff a)
    (.+^) = liftA2 (.+^)
    (.-.) = liftA2 (.-.)

-- TODO undecidable
instance (HasGetPitch a, HasSetPitch a b
                ) => 
                HasSetPitch (Behavior a) (Behavior b) where
    type SetPitch (Behavior p) (Behavior a) = Behavior (SetPitch p a)
    __mapPitch = foo
        
        -- where
        --     f' :: B (P a) -> B (P b)
        --     f' = f
        -- 
        --     x' :: B a
        --     x' = x
        -- 
        --     r :: B b
        --     r = undefined   

foo :: (HasGetPitch a, HasSetPitch a b, Applicative f) => (f (P a) -> f (P b)) -> f a -> f b
foo f a = liftA2 __setPitch (f $ (__getPitch) <$> a) a




-- liftA2 __setPitch (f ((__getPitch) <$> a)) a
-- foo a = 
    -- where ap = fmap (__getPitch) ap

    -- __mapPitch f = fmap (spitch %~ f)

instance (HasSetPitch a b
            -- , Transformable (Music.Score.Pitch a)
            -- , Transformable (Music.Score.Pitch b)
            ) => 
                HasSetPitch (First a) (First b) where
    type SetPitch p (First a) = First (SetPitch p a)
    __mapPitch f = fmap (spitch %~ f) 

type Isom a b = (a -> b, b -> a)
-- type Yup  dummy = forall a b . (a ~ b, dummy ~ dummy)
-- type Nope dummy = forall a b . (a ~ b, b ~ Int, a ~ Float)

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


instance IsPitch a => IsPitch (Behavior a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Behavior a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (Behavior a) where
    fromDynamics = pure . fromDynamics


instance Functor First where
    fmap f (First x) = First (f x)
instance Applicative First where
    pure x = First x
    First f <*> First x = First (f x)

instance Functor Last where
    fmap f (Last x) = Last (f x)
instance Applicative Last where
    pure x = Last x
    Last f <*> Last x = Last (f x)

instance IsPitch a => IsPitch (First a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (First a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (First a) where
    fromDynamics = pure . fromDynamics

instance IsPitch a => IsPitch (Last a) where
    fromPitch = pure . fromPitch
instance IsInterval a => IsInterval (Last a) where
    fromInterval = pure . fromInterval
instance IsDynamics a => IsDynamics (Last a) where
    fromDynamics = pure . fromDynamics

instance Tiable a => Tiable (Behavior a) where toTied x = (x,x)
instance Tiable a => Tiable (First a) where toTied x = (x,x)
instance HasLilypond a => HasLilypond (Behavior a) where
    getLilypond d = getLilypond d . (? 0)
instance HasLilypond a => HasLilypond (First a) where
    getLilypond d = getLilypond d . getFirst
instance HasMidi a => HasMidi (Behavior a) where
    getMidi = getMidi . (? 0)
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
