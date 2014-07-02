
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

----------------------------------------------------

import Control.Lens
import Music.Time

type MVoice a = Voice (Maybe a)

class HasPhrases s t a b | s -> a, t -> b, s b -> t, t a -> s where
  mvoices :: Traversal s t (MVoice a) (MVoice b)
type Ctxt a = (Maybe a, a, Maybe a)

type IdC = (() ~ ())


data CynamicNotation
type instance Cynamic Double = Double
type instance Cynamic (Ctxt a) = Cynamic a
type instance Cynamic CynamicNotation = CynamicNotation

rewriteCyn
  :: (Real a1,
      HasPhrases
        (SetCynamic CynamicNotation b)
        (SetCynamic CynamicNotation b)
        uf
        uf,
      HasPhrases
        a
        b
        uf1
        (SetCynamic
           (Maybe (Cynamic uf1), Cynamic uf1, Maybe (Cynamic uf1)) uf1),
      HasCynamics uf uf, HasCynamics b (SetCynamic CynamicNotation b),
      HasCynamic uf1 uf1,
      HasCynamic
        uf1
        (SetCynamic
           (Maybe (Cynamic uf1), Cynamic uf1, Maybe (Cynamic uf1)) uf1),
      Cynamic b ~ (Maybe a1, a1, Maybe a1),
      Cynamic
        (SetCynamic
           (Maybe (Cynamic uf1), Cynamic uf1, Maybe (Cynamic uf1)) uf1)
      ~ (Maybe (Cynamic uf1), Cynamic uf1, Maybe (Cynamic uf1)),
      Cynamic (SetCynamic CynamicNotation b) ~ CynamicNotation,
      Cynamic uf ~ CynamicNotation,
      SetCynamic CynamicNotation uf ~ uf) =>
     a -> SetCynamic CynamicNotation b
rewriteCyn = removeCloseCynMarks . over cynamics notateCynamic . addCynCon

removeCloseCynMarks :: (HasPhrases s s a a, HasCynamics a a, Cynamic a ~ CynamicNotation, a ~ SetCynamic (Cynamic a) a) => s -> s
removeCloseCynMarks = undefined

notateCynamic :: (Ord a, Real a) => Ctxt a -> CynamicNotation
notateCynamic = undefined

addCynCon :: (HasPhrases s t a b, HasCynamic a a, HasCynamic a b, Cynamic a ~ d, Cynamic b ~ Ctxt d) => s -> t
addCynCon = undefined

-- |
-- Cynamics type.
--
type family Cynamic (s :: *) :: *

-- |
-- Cynamic type.
--
type family SetCynamic (b :: *) (s :: *) :: *

-- |
-- Class of types that provide a single cynamic.
--
class (HasCynamics s t) => HasCynamic s t where

  -- | Access a single cynamic.
  cynamic :: Lens s t (Cynamic s) (Cynamic t)


type CynamicLensLaws' s t a b = (
  Cynamic (SetCynamic a s) ~ a,
  SetCynamic (Cynamic t) s ~ t,
  SetCynamic a (SetCynamic b s) ~ SetCynamic a s
  )

type CynamicLensLaws s t = CynamicLensLaws' s t (Cynamic s) (Cynamic t)

-- |
-- Class of types that provide a cynamic traversal.
--
class (
  Transformable (Cynamic s),
  Transformable (Cynamic t),
  -- SetCynamic (Cynamic t) s ~ t,
  CynamicLensLaws s t
  ) => HasCynamics s t where

  -- | Access all cynamics.
  cynamics :: Traversal s t (Cynamic s) (Cynamic t)

type HasCynamic'  a = HasCynamic  a a
type HasCynamics' a = HasCynamics a a

cynamics' :: HasCynamics' s => Traversal' s (Cynamic s)
cynamics' = cynamics
