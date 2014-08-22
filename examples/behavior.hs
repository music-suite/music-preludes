
import Music.Prelude

si :: RealFrac a => a -> Interval
si t = (spell usingSharps $ (round t :: Semitones))

sp :: RealFrac a => a -> Pitch
sp x = c .+^ si x

main = open $ set pitches' (fmap sp (stretch 10 $ sine*10)) $ times 20 c