
import Control.Lens
import Control.Arrow
import Music.Prelude.Basic

withOrigin x = (.-. x)
mapIf p f = uncurry mplus . first f . mpartition p
unb = (! 0)
mark = above _P8 -- TODO use color or similar

markPerfect   = text "Perfect consonances"   . mapIf (isPerfectConsonance   . unb . withOrigin c . (head . toListOf pitches)) mark
markImperfect = text "Imperfect consonances" . mapIf (isImperfectConsonance . unb . withOrigin c . (head . toListOf pitches)) mark
markDiss      = text "Dissonances"           . mapIf (isDissonance . unb . withOrigin c          . (head . toListOf pitches)) mark

main = open $ showAnnotations $ rcat [
    markPerfect   $ scat [c..c'],
    markImperfect $ scat [c..c'],
    markDiss      $ scat [c..c']    
  ]
