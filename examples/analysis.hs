
{-# LANGUAGE TypeFamilies #-}

import Music.Prelude.Standard
import qualified Music.Score as Score
import Data.Colour.Names (red)

markIf :: (HasColor a, HasPitches' a, Score.Pitch a ~ Behavior Pitch) => (Interval -> Bool) -> Score a -> Score a
markIf p     = mapIf (\x -> p $ withOrigin c $ unb $ x ^?! pitches) mark
  where
    mark         = color red
    mapIf p f    = uncurry mplus . over _1 f . mpartition p
    unb          = (! 0)
    withOrigin x = (.-. x)

markPerfect   = text "Perfect consonances"   . markIf isPerfectConsonance
markImperfect = text "Imperfect consonances" . markIf isImperfectConsonance
markDiss      = text "Dissonances"           . markIf isDissonance


main = openLilypond $ asScore $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
