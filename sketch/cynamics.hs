
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
newtype Ctxt a = Ctxt (Maybe a, a, Maybe a)

type IdC = (() ~ ())


data CynamicNotation
type instance Cynamic Double = Double
type instance Cynamic (Ctxt a) = Cynamic a
type instance Cynamic CynamicNotation = CynamicNotation


-- |
-- Cynamics type.
--
type family Cynamic (s :: *) :: *

-- |
-- Class of types that provide a cynamic traversal.
--
class (
  Transformable (Cynamic s),
  Transformable (Cynamic t),
  a ~ Cynamic s,
  b ~ Cynamic t,
  IdC
  ) => HasCynamics s t a b | s -> a, t -> b, s b -> t, t a -> s where

  -- | Access all cynamics.
  cynamics :: Traversal s t a b

-- |
-- Class of types that provide a single cynamic.
--
class (HasCynamics s t a b) => HasCynamic s t a b | s -> a, t -> b, s b -> t, t a -> s where

  -- | Access a single cynamic.
  cynamic :: Lens s t a b


type HasCynamic'  s a = HasCynamic  s s a a
type HasCynamics' s a = HasCynamics s s a a
type HasPhrases'  s a = HasPhrases s s a a

cynamics' :: HasCynamics' s a => Traversal' s a
cynamics' = cynamics

rewriteCyn
  :: (
      HasPhrases s s2 a b, HasCynamic' a dyn, HasCynamic a b dyn (Ctxt dyn),
      Real dyn, HasCynamics s2 t (Ctxt dyn) CynamicNotation,
      HasPhrases' t c, HasCynamics' c CynamicNotation
      ) =>
     s -> t
rewriteCyn = removeCloseCynMarks . over cynamics notateCynamic . addCynCon

removeCloseCynMarks :: (HasPhrases' s a, HasCynamics' a CynamicNotation) => s -> s
removeCloseCynMarks = undefined

notateCynamic :: Real a => Ctxt a -> CynamicNotation
notateCynamic = undefined

addCynCon :: (HasPhrases s t a b, HasCynamic' a d, HasCynamic a b d (Ctxt d)) => s -> t
addCynCon = undefined
