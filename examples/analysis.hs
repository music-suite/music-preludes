
import Control.Arrow
import Data.AffineSpace.Relative
import Music.Prelude.Basic

withOrigin x = (.-. x)
mapIf p f = uncurry mplus . first f . mpartition p

markPerfect   = text "Perfect consonances"   . mapIf (isPerfectConsonance   . withOrigin c . getPitch) accentAll
markImperfect = text "Imperfect consonances" . mapIf (isImperfectConsonance . withOrigin c . getPitch) accentAll
markDiss      = text "Dissonances" . mapIf (isDissonance . withOrigin c . getPitch) accentAll

main = open $ showAnnotations $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
