
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

import Music.Score

{-
  Assume that Music is a type function that returns the underlying music
  representation for a given backend.

  Then, for each backend B we need to provide a function
    s a -> Music B
  where s is some score-like type constructor, and a is some note-like type.
  From a we need to fetch each aspect:
    pitch
    dynamic
    articulation
    part
  and convert it to the relevant representation of that aspect in B.
  For example with lilypond we need to convert to LilypondPitch, LilypondDynamic etc.
  Then we need to take s and convert it into some kind of fold for the musical types
  (usually a set of parallel, seequential compositions). Apply the folds, and we're done.
  
  
  
  
  

  chord
  behavior
  tie
  slide

  tremolo
  harmonic
  text
  clef
-}

-- -- Export token type
-- class Export b where
--   type EChord b
--   emptyN  :: b -> EChord b
-- 
-- class ExportN b a where
--   exportN  :: b -> a   -> EChord b -> EChord b
--   exportNs :: b -> [a] -> EChord b -> EChord b
-- 
-- class Functor s => ExportS b s where
--   type EScore b
--   exportS :: b -> s a -> EScore b a
-- 
-- export :: (Functor (EScore b), Export b, ExportS b s, ExportN b a) => b -> s a -> EScore b (EChord b)
-- export = \b x -> (exportS b . fmap (\a -> exportN b a (emptyN b))) x
-- 
-- 


class Functor (BackendScore b) => HasBackend b where
  type BackendScore b :: * -> *
  type BackendNoteRest b :: *
  type BackendMusic b :: *
  finalize :: b -> BackendScore b (BackendNoteRest b) -> BackendMusic b
  
class (HasBackend b, Functor s) => HasBackendScore b s where
  exportScore :: b -> s a -> BackendScore b a

class (HasBackend b) => HasBackendNoteRest b a where
  exportNote :: b -> a -> BackendNoteRest b


export :: (HasBackendScore b s, HasBackendNoteRest b a) => b -> s a -> BackendMusic b
export b = finalize b . 
  exportScore b 
  . 
  fmap (exportNote b) 

data Ly
instance HasBackend Ly where
  type BackendScore Ly = []
  type BackendNoteRest  Ly = (Int, Int)
  type BackendMusic Ly = [(Int, Int)]
  finalize _ = id
instance HasBackendScore Ly [] where
  exportScore _ = id
instance HasBackendNoteRest Ly Int where
  exportNote _ p = (0,p)
instance HasBackendNoteRest Ly a => HasBackendNoteRest Ly (DynamicT Int a) where
  exportNote b (DynamicT (d,p)) = set _1 d $ exportNote b p

{-
  export (undefined::Ly) [DynamicT (4::Int,3::Int)]
-}  
