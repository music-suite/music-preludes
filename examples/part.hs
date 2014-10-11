
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Arvo Pärt: Cantus in Memory of Benjamin Britten (1977)
-- 
-- Inspired by the Abjad transcription
--
import Music.Prelude hiding (open)
import qualified Music.Score as Score

ensemble :: [Part]
ensemble = [solo tubularBells] <> (divide 2 (tutti violin)) <> [tutti viola] <> [tutti cello] <> [tutti doubleBass]

withTintin :: (HasPitches' a, Score.Pitch a ~ Behavior Pitch) => Pitch -> Score a -> Score a
withTintin p x = x <> tintin p x

-- | 
-- Given the a melody voice return the tintinnabuli voice.
-- 
tintin :: (HasPitches' a, Score.Pitch a ~ Behavior Pitch) => Pitch -> Score a -> Score a
tintin tonic = pitches . mapped %~ relative tonic tintin'

-- | 
-- Given the melody interval (relative tonic), returns the tintinnabular voice interval. 
--
-- That is return the highest interval that is a member of the tonic minorTriad in any octave
-- which is also less than the given interval 
--
tintin' :: Interval -> Interval
tintin' melInterval 
    | isNegative melInterval = error "tintin: Negative interval"
    | otherwise = last $ takeWhile (< melInterval) $ tintinStandardNotes
    where
        tintinStandardNotes = concat $ iterate (fmap (+ _P8)) minorTriad
        minorTriad = [_P1,m3,_P5]

fallingScale :: [Score StandardNote]
fallingScale = [a',g'..a_]

fallingScaleSect :: Int -> [Score StandardNote]
fallingScaleSect n = {-fmap (annotate (show n)) $-} take n $ fallingScale

mapEvensOdds :: (a -> b) -> (a -> b) -> [a] -> [b]
mapEvensOdds f g [] = []
mapEvensOdds f g [a] = [f a]
mapEvensOdds f g (a : b : cs) = f a : g b : mapEvensOdds f g cs

mainSubject :: Score StandardNote
mainSubject = stretch (1/6) $ asScore $ scat $ mapEvensOdds (accent . (^*2)) id $ concatMap fallingScaleSect [1..30]

bell :: Score StandardNote
bell = let
    cue :: Score (Maybe StandardNote)
    cue = stretchTo 1 (rest |> a) 
    in parts' .~ (ensemble !! 0) $ text "l.v." $ mcatMaybes $ times 40 $ scat [times 3 $ scat [cue,rest], rest^*2]

strings :: Score StandardNote
strings = strings_vln1 <> strings_vln2 <> strings_vla <> strings_vc <> strings_db
  where
    strings_vln1 = clef GClef $ parts' .~ (ensemble !! 1) $ up (_P8^*1)   $ strings_cue
    strings_vln2 = clef GClef $ parts' .~ (ensemble !! 2) $ up (_P8^*0)   $ stretch 2 strings_cue
    strings_vla  = clef CClef $ parts' .~ (ensemble !! 3) $ down (_P8^*1) $ stretch 4 strings_cue
    strings_vc   = clef FClef $ parts' .~ (ensemble !! 4) $ down (_P8^*2) $ stretch 8 strings_cue
    strings_db   = clef FClef $ parts' .~ (ensemble !! 5) $ down (_P8^*3) $ stretch 16 strings_cue

    strings_cue  = delay (1/2) $ withTintin (down (_P8^*4) $ asPitch a) $ mainSubject

music :: Score StandardNote
music = meta $ stretch (3/2) $ {-before 60-} (mempty <> bell <> delay 6 strings)
    where
        meta = id
          . title "Cantus in Memoriam Benjamin Britten" 
          . composer "Arvo Pärt" 
          . timeSignature (6/4) 
          . tempo (metronome (1/4) 120)

openBook :: Score StandardNote -> IO ()
openBook = openLilypond' LyScoreFormat

main :: IO ()
main = openBook music
