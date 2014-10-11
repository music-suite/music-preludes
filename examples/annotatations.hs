
{-
  Annotate a melody with intervals.

  Written by Carlo Nucera
-}
module Annotations where

import Music.Prelude.Basic hiding  (Interval)
import Music.Pitch (Interval)

main :: IO ()
main = openLilypond . showAnnotations' ""
     . intervalAnnotations subjectDiff
     . scat $ map (fromPitch'.pure) subject

subject :: [Pitch]
subject = [c, d, f, e, f, g, a, g, e, d, c]

subjectDiff :: [Interval]
subjectDiff = zipWith (.-.) (tail subject) subject

intervalAnnotations :: [Interval] -> (Score BasicNote -> Score BasicNote)
intervalAnnotations = foldr1 (.) . zipWith notate (map spanify [0..])
  where
    spanify :: Duration -> Span
    spanify t = (0 .+^ t) >-> 1

    notate :: Span -> Interval -> (Score BasicNote -> Score BasicNote)
    notate s n = annotateSpan s ("       " ++ showIntervalName n)
 
    showIntervalName = filter (/= '_') . show

