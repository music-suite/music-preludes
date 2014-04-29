
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module L where

import Music.Score
import Music.Score.Util (tripl, untripl, through)
import Music.Score.Convert
import Music.Prelude.Basic
import Data.Maybe
import qualified Data.List as List
-- import Control.Lens hiding (transform, parts, )


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

{-
extracted :: (Ord (Part a), HasPart' a) => Iso' (Score a) [Score a]
extracted = iso extractParts mconcat

-- TODO what to use instead of elements to select parts?
extracted' :: (Ord (Part a), HasPart' a) => Iso' (Score a) [(Part a, Score a)]
extracted' = iso extractParts' $ mconcat . fmap (uncurry $ set parts)

-}

-- TODO failure
-- TODO why Transformable?
singleMVoice :: Transformable a => Prism' (Score a) (Voice (Maybe a))
singleMVoice = iso scoreToVoice voiceToScore'


-- Traverse writing to all elements *except* first and last
_mid = _tail._init

firstS :: Transformable a => Traversal' (Score a) a
firstS = (notes._head.getNote)

lastS :: Transformable a => Traversal' (Score a) a
lastS = (notes._last.getNote)

firstV :: Transformable a => Traversal' (Voice a) a
firstV = (eventsV._head._2)

middleV :: Transformable a => Traversal' (Voice a) a
middleV = (eventsV._mid.traverse._2)

lastV :: Transformable a => Traversal' (Voice a) a
lastV = (eventsV._last._2)




-- TODO
mvoicePhrases :: Iso' (Voice (Maybe a)) [Either Duration (Voice a)]
mvoicePhrases = iso mvoiceToPhrases phrasesToMVoice

phrasesToMVoice :: [Either Duration (Voice a)] -> (Voice (Maybe a))
phrasesToMVoice = mconcat . fmap (either restToVoice phraseToVoice)

mvoiceToPhrases :: (Voice (Maybe a)) -> [Either Duration (Voice a)]
mvoiceToPhrases =
  map ( bimap voiceToRest voiceToPhrase 
      . bimap (^.from unsafeEventsV) (^.from unsafeEventsV) ) 
   . groupDiff' (isJust . snd) 
   . view eventsV

  where
restToVoice :: Duration -> Voice (Maybe a)
restToVoice d = stretch d $ pure Nothing

phraseToVoice :: Voice a -> Voice (Maybe a)
phraseToVoice = fmap Just

voiceToRest :: Voice (Maybe a) -> Duration
voiceToRest = sumOf (eventsV.each._1) . fmap (assert "isNothing" isNothing)
-- TODO just _duration

voiceToPhrase :: Voice (Maybe a) -> Voice a
voiceToPhrase = fmap fromJust

assert t p x = if p x then x else error ("assertion failed: " ++ t)
        

-- |
-- Group contigous sequences matching/not-matching the predicate.
--
-- >>> groupDiff (== 0) [0,1,2,3,5,0,0,6,7]
-- [[0],[1,2,3,5],[0,0],[6,7]]
--
groupDiff :: (a -> Bool) -> [a] -> [[a]]
groupDiff p []     = []
groupDiff p (x:xs)
  | p x       = (x : List.takeWhile p         xs) : groupDiff p (List.dropWhile p         xs)
  | not (p x) = (x : List.takeWhile (not . p) xs) : groupDiff p (List.dropWhile (not . p) xs)

groupDiff' :: (a -> Bool) -> [a] -> [Either [a] [a]]
groupDiff' p []     = []
groupDiff' p (x:xs)
  | not (p x) = Left  (x : List.takeWhile (not . p) xs) : groupDiff' p (List.dropWhile (not . p) xs)
  | p x       = Right (x : List.takeWhile p         xs) : groupDiff' p (List.dropWhile p         xs)


eventsV :: Lens (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
eventsV = unsafeEventsV
  --(stretcheds.through (from stretched) (from stretched))

unsafeEventsV :: Iso (Voice a) (Voice b) [(Duration, a)] [(Duration, b)]
unsafeEventsV = iso (map (^.from stretched).(^.stretcheds)) ((^.voice).map (^.stretched))














-- also:
-- mvoicePhrases2 :: Iso' (Voice (Maybe a)) (Voice (Maybe (Voice a)))
-- mvoicePhrases2 = undefined


-- This is the famous voice traversal!
phr :: (Ord (Part a), HasPart' a, Transformable a) => Traversal' (Score a) (Voice a)
phr = extracted . each . singleMVoice . mvoicePhrases . each . _Right

-- More generally:

phrases :: HasVoices a b => Traversal' a (Voice b)
phrases = mvoices . mvoicePhrases . each . _Right


class HasVoices a b | a -> b where
  mvoices :: Traversal' a (Voice (Maybe b))
instance HasVoices (Voice (Maybe a)) a where
  mvoices = id
instance (HasPart' a, Transformable a, Ord (Part a)) => HasVoices (Score a) a where
  mvoices = extracted . each . singleMVoice