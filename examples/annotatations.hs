
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
     . scat $ map reify subject

subject :: [BasicPitch]
subject = [c, d, f, e, f, g, a, g, e, d, c]

subjectDiff :: [Interval]
subjectDiff = zipWith (.-.) (tail subject) subject

reify :: BasicPitch -> Score BasicNote
reify = (`up` c) . (.-. c)

intervalAnnotations :: [Interval] -> (Score BasicNote -> Score BasicNote)
intervalAnnotations = foldr1 (.) . zipWith notate (map spanify [0..])
  where
    spanify :: Duration -> Span
    spanify t = (origin .+^ t) >-> 1

    notate :: Span -> Interval -> (Score BasicNote -> Score BasicNote)
    notate s n = annotateSpan s ("       " ++ showIntervalName n)
 
    showIntervalName = filter (/= '_') . show

