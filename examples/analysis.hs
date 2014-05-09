
{-# LANGUAGE NoMonomorphismRestriction #-}

-- import Control.Lens
import Control.Arrow
import Music.Prelude.Basic

withOrigin x = (.-. x)
mapIf p f = uncurry mplus . first f . mpartition p
unb = (! 0)
mark = text "!" -- TODO use color or similar

markPerfect   = text "Perfect consonances"   . mapIf (\x -> isPerfectConsonance $ withOrigin c $ unb $ x ^?! pitches) mark
markImperfect = text "Imperfect consonances" . mapIf (\x -> isImperfectConsonance $ withOrigin c $ unb $ x ^?! pitches) mark
markDiss      = text "Dissonances"           . mapIf (\x -> isDissonance $ withOrigin c $ unb $ x ^?! pitches) mark

main = open $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
