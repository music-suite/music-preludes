
import Control.Lens
import Control.Arrow
import Music.Prelude.Basic

withOrigin x = (.-. x)
mapIf p f = uncurry mplus . first f . mpartition p

markPerfect   = text "Perfect consonances"   . mapIf (isPerfectConsonance   . withOrigin c . view pitch') accentAll
markImperfect = text "Imperfect consonances" . mapIf (isImperfectConsonance . withOrigin c . view pitch') accentAll
markDiss      = text "Dissonances" . mapIf (isDissonance . withOrigin c . view pitch') accentAll

main = open $ showAnnotations $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
