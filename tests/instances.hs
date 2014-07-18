
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding ((**))

import Data.Ord (comparing)
import Music.Prelude hiding (elements, unit, (**), Note)
-- import Data.VectorSpace hiding (Sum)
import Music.Time (Note)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Data.Typeable
import Data.Semigroup
import Control.Monad
import Control.Applicative
import Control.Comonad
import qualified Data.List

type Checkable  a     = (Eq a, Show a, Arbitrary a)


prop_semigroup :: (Checkable a, Semigroup a) => a -> Property
prop_semigroup typ = property assoc
  where
    assoc a b c = (a <> b) <> c === a <> (b <> c .: typ)

_Monoid :: (Checkable a, Semigroup a, Monoid a) => a -> Property
_Monoid typ = idL .&&. idR .&&. assoc    
  where
    idL   m     = m <> mempty   === m .: typ
    idR   m     = mempty <> m   === m .: typ
    assoc a b c = (a <> b) <> c === a <> (b <> c .: typ)

{-
prop_functor :: (Functor f, Eq (f ()), Arbitrary (f ()), Show (f ()), Eq (f c), Arbitrary (f a), Show (f a)) => f () -> (b -> c) -> (a -> b) -> Property
prop_functor typ f g = fc .&&. fi
  where
    fi x = x == (sameType typ x)
    fc x = (fmap f . fmap g) x == (fmap (f . g) $ sameType1 typ x)

-- prop_applicative typ

unit :: Applicative f => f ()
unit = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
a ** b = liftA2 (,) a b


-- prop_app typ = appLId .&&. appRId .&&. appAssoc
--   where
--     appLId v       = property $ unit ** v == fmap ((),) (sameType typ v)
--     appRId u       = property $ u ** unit == fmap (,()) (sameType typ u)
--     appAssoc u v w = property $ u ** (v ** w) == (fmap unass $ (u ** v) ** sameType typ w)
    
appLId :: (Eq (f ((), b)), Applicative f) => f b -> Property
appLId v = property $ unit ** v == fmap ((),) v

appRId :: (Eq (f (a, ())), Applicative f) => f a -> Property
appRId u = property $ u ** unit == fmap (,()) u

appAssoc :: (Eq (f (a, (b, c))), Applicative f) => f a -> f b -> f c -> Property
appAssoc u v w = property $ u ** (v ** w) == (fmap unass $ (u ** v) ** w)

unass :: ((a, b), c) -> (a, (b, c))
unass = \((a, b), c) -> (a, (b, c))

-}

{-
  transform mempty = id
  transform (s <> t) = transform s . transform t
  transform (s <> negateV s) = id
-}
_Transformable :: (Checkable a, Transformable a) => a -> Property
_Transformable typ = te .&&. tc .&&. tn
  where
    te x     = True                               ==> transform mempty x === x .: typ
    tc s t x = isForwardSpan s && isForwardSpan t ==> transform (s <> t) x === transform s (transform t $ x .: typ)
    tn s x   = isForwardSpan s                    ==> transform (s <> negateV s) x === x .: typ

_HasPosition :: (Checkable a, Transformable a, HasPosition a) => a -> Property
_HasPosition typ = ond .&&. ofd .&&. sd .&&. cd
  where
    ond n a = n /= 0 ==> _onset (delay n $ a .: typ)       === _onset a  .+^ n
    ofd n a = n /= 0 ==> _offset (delay n $ a .: typ)      === _offset a .+^ n
    sd n a  = n /= 0 ==> _duration (stretch n $ a .: typ)  === n * _duration a
    cd n a  = n /= 0 ==> _duration (stretch (1/n) $ a .: typ) === (1/n) * _duration a
    -- TODO more general

data BadMonoid = BM | BM2
  deriving (Eq, Ord, Show, Typeable)
instance Monoid BadMonoid where
  mempty = BM
  _ `mappend` _ = BM
instance Semigroup BadMonoid where
  (<>) = mappend
instance Arbitrary BadMonoid where
  arbitrary = elements [BM, BM2]


data BadFunctor a = BF1 | BF2
  deriving (Eq, Ord, Show, Typeable)
instance Functor BadFunctor where
  fmap f BF1 = BF2 -- lawless
  fmap f BF2 = BF1
instance Applicative BadFunctor where
  pure _ = BF1
  f <*> BF1 = BF2
  f <*> BF2 = BF1
instance Arbitrary (BadFunctor a) where
  arbitrary = elements [BF2, BF1]



sameType :: a -> a -> a
sameType _ x = x

infixl 9 .:
x .: typ = sameType typ x

sameType1 :: f a -> f b -> f b
sameType1 _ x = x




instance Arbitrary Time where
  arbitrary = fmap toTime (arbitrary::Gen Double)
instance Arbitrary Duration where
  arbitrary = fmap toDuration (arbitrary::Gen Double)
instance Arbitrary Span where
  arbitrary = liftA2 (<->) arbitrary arbitrary
instance Arbitrary a => Arbitrary (Delayed a) where
  arbitrary = fmap (view delayed) arbitrary
instance Arbitrary a => Arbitrary (Stretched a) where
  arbitrary = fmap (view stretched) arbitrary
instance Arbitrary a => Arbitrary (Note a) where
  arbitrary = fmap (view note) arbitrary

instance Arbitrary a => Arbitrary (Voice a) where
  arbitrary = fmap (view voice) arbitrary  
-- instance Arbitrary a => Arbitrary (Chord a) where
  -- arbitrary = fmap (view chord) arbitrary  
instance Arbitrary a => Arbitrary (Score a) where
  arbitrary = fmap (view score) arbitrary  


instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary


-- TODO move
instance Ord a => Ord (Note a) where
  x < y = x^.from note < y^.from note
instance Eq a => Eq (Score a) where
  x == y = Data.List.sortBy (comparing (^.era)) (x^.notes) == Data.List.sortBy (comparing (^.era)) (y^.notes)
instance Show (Score a) where
  show _ = "{{Score}}"


-- main = quickCheck $ \() () -> True

#define A_TEST(EXPR) (testProperty "EXPR" $ EXPR)
#define I_TEST(CLASS,TYPE) (testProperty "instance CLASS TYPE" $ (CLASS (undefined::TYPE)))

main = defaultMain $ testGroup "Instances" $ [
  I_TEST(_Monoid, ()),
  I_TEST(_Monoid, Sum Int),
  I_TEST(_Monoid, [Int]),
  I_TEST(_Monoid, Span),
  -- I_TEST(_Monoid, Note ()),
  -- I_TEST(_Monoid, Stretched ()),
  -- I_TEST(_Monoid, Delayed ()),
  I_TEST(_Monoid, Voice Int),
  -- I_TEST(_Monoid, Chord Int),
  I_TEST(_Monoid, Score Int),

  I_TEST(_Transformable, Time),
  I_TEST(_Transformable, Duration),
  I_TEST(_Transformable, Span),
  I_TEST(_Transformable, Int),
  I_TEST(_Transformable, Double),
  I_TEST(_Transformable, Note Int),
  I_TEST(_Transformable, Note Double),
  I_TEST(_Transformable, Stretched Int),
  I_TEST(_Transformable, Stretched Double),
  I_TEST(_Transformable, Delayed Int),
  I_TEST(_Transformable, Delayed Double),

  I_TEST(_Transformable, Voice Integer),
  -- I_TEST(_Transformable, Chord Integer),
  I_TEST(_Transformable, Score Integer),

  I_TEST(_Transformable, [Voice Integer]),
  -- I_TEST(_Transformable, [Chord] Integer),
  I_TEST(_Transformable, [Score Integer]),

  I_TEST(_HasPosition, Time),
  -- I_TEST(_HasPosition, Duration),
  I_TEST(_HasPosition, Span),
  -- I_TEST(_HasPosition, Int),
  -- I_TEST(_HasPosition, Double),
  I_TEST(_HasPosition, Note Int),
  I_TEST(_HasPosition, Note Double),
  -- I_TEST(_HasPosition, Stretched Int),
  -- I_TEST(_HasPosition, Stretched Double),
  I_TEST(_HasPosition, Delayed Int),
  I_TEST(_HasPosition, Delayed Double),

  -- I_TEST(_HasPosition, Chord Integer),
  I_TEST(_HasPosition, Score Integer),
  -- I_TEST(_HasPosition, [Chord Integer]),
  I_TEST(_HasPosition, [Score Integer]),
 
  I_TEST(_Transformable, Stretched [Note Int])

  ]
