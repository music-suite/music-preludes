
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module L where

-- import Music.Score
-- import Music.Score.Util (tripl, untripl, through)
import Music.Score.Convert
import Music.Prelude.Basic
import Control.Lens hiding (transform, parts)


{-
-- TODO poly

tripped :: Iso ((a, b), c) ((a', b'), c') (a, b, c) (a', b', c')
tripped = iso tripl untripl

event :: Lens (Note a) (Note b) (Time, Duration, a) (Time, Duration, b)
event = from note . alongside delta id . tripped

events2 :: Transformable a => Lens (Score a) (Score b) [(Time, Duration, a)] [(Time, Duration, b)]
events2 = notes . through event event
-}

-- over (extracted . elements even) (up m2) $ rcat [c,c,c,c]
-- toListOf (extracted.each.singleVoice)

extracted :: (Ord (Part a), HasPart' a) => Iso' (Score a) [Score a]
extracted = iso extractParts mconcat

-- TODO what to use instead of elements to select parts?
extracted' :: (Ord (Part a), HasPart' a) => Iso' (Score a) [(Part a, Score a)]
extracted' = iso extractParts' $ mconcat . fmap (uncurry $ set parts)


-- TODO failure
-- TODO why Transformable?
singleMVoice :: Transformable a => Prism' (Score a) (Voice (Maybe a))
singleMVoice = iso scoreToVoice voiceToScore'

-- TODO
mvoicePhrases :: Iso' (Voice (Maybe a)) [Either Duration (Voice a)]
mvoicePhrases = undefined
-- also:
mvoicePhrases2 :: Iso' (Voice (Maybe a)) (Voice (Maybe (Voice a)))
mvoicePhrases2 = undefined


-- This is the famous voice traversal!
phr :: (Ord (Part a), HasPart' a, Transformable a) => Traversal' (Score a) (Voice a)
phr = extracted . each . singleMVoice . mvoicePhrases . each . _Right

-- More generally:

phrases :: HasVoices a a => Traversal' a (Voice a)
phrases = mvoices . mvoicePhrases . each . _Right


class HasVoices a b where
  mvoices :: Traversal' a (Voice (Maybe b))
instance HasVoices (Voice (Maybe a)) a where
  mvoices = id
instance (HasPart' a, Transformable a, Ord (Part a)) => HasVoices (Score a) a where
  mvoices = extracted . each . singleMVoice