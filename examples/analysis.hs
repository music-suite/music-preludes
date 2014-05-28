
{-# LANGUAGE NoMonomorphismRestriction #-}

-- import Control.Lens
import Control.Arrow
import Music.Prelude.Standard
import Data.Colour.Names as Color

withOrigin x = (.-. x)
mapIf p f = uncurry mplus . first f . mpartition p
unb = (! 0)
-- mark = text "!" -- TODO use color or similar
mark = color Color.red

markPerfect   = text "Perfect consonances"   . mapIf (\x -> isPerfectConsonance $ withOrigin c $ unb $ x ^?! pitches) mark
markImperfect = text "Imperfect consonances" . mapIf (\x -> isImperfectConsonance $ withOrigin c $ unb $ x ^?! pitches) mark
markDiss      = text "Dissonances"           . mapIf (\x -> isDissonance $ withOrigin c $ unb $ x ^?! pitches) mark

main = openLilypond $ asScore $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
