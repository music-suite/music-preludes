
{-# LANGUAGE
    OverloadedStrings #-}

import Music.Prelude.Standard hiding (tempo, open, play, openAndPlay)
import Control.Concurrent.Async
import Control.Applicative
import Data.AffineSpace.Relative
import System.Process (system)

{-    
    Pärt: Cantus in Memory of Benjamin Britten
    Copied from Abjad

    TODO tempo
    TODO proper 6/4
-}

main :: IO ()
main = open score

play, open, openAndPlay :: Score Note -> IO ()   
tempo = 80
play x = openAudio $ stretch ((60*4)/tempo) $ fixClefs $ x
open x = openLy' Score $ fixClefs $ x
openAndPlay x = play x `concurrently_` open x

ensemble :: [Part]
ensemble = [solo tubularBells] <> (divide 2 (tutti violin)) <> [tutti viola] <> [tutti cello] <> [tutti bass]

score :: Score Note
score = meta $ dynamics ppp $ before 60 (bell <> delay 6 strings)
    where
        meta = title "Cantus in Memoriam Benjamin Britten" . composer "Arvo Pärt"

withTintin :: Pitch -> Score Note -> Score Note
withTintin p x = x {-<> tintin p x-}

-- | Given the melody voice return the tintinnabular voice.
tintin :: Pitch -> Score Note -> Score Note
tintin tonic = modifyPitch (relative tonic tintin')

-- | 
-- Given the melody interval (relative tonic), returns the tintinnabular voice interval. 
--
-- That is return the highest interval that is a member of the tonic minorTriad in any octave
-- which is also less than the given interval 
--
tintin' :: Interval -> Interval
tintin' melInterval 
    | isNegative melInterval = error "tintin: Negative interval"
    | otherwise = last $ takeWhile (< melInterval) $ tintinNotes
    where
        tintinNotes = concat $ iterate (fmap (+ _P8)) minorTriad
        minorTriad = [_P1,m3,_P5]


bell :: Score Note
bell = let
    cue = stretchTo 1 (rest |> a) 
    in setPart (ensemble !! 0) $ text "l.v." $ removeRests $ times 40 $ scat [times 3 $ scat [cue,rest], rest^*2]

strings :: Score Note
strings = strings_vln1 <> strings_vln2 <> strings_vla <> strings_vc <> strings_db

strings_vln1 = clef GClef $ setPart (ensemble !! 1) $ octavesUp   1 $ strings_cue
strings_vln2 = clef GClef $ setPart (ensemble !! 2) $ octavesDown 0 $ stretch 2 strings_cue
strings_vla  = clef CClef $ setPart (ensemble !! 3) $ octavesDown 1 $ stretch 4 strings_cue
strings_vc   = clef FClef $ setPart (ensemble !! 4) $ octavesDown 2 $ stretch 8 strings_cue
strings_db   = clef FClef $ setPart (ensemble !! 5) $ octavesDown 3 $ stretch 16 strings_cue
strings_cue = delay (1/2) $ withTintin (octavesDown 4 a) mainSubject

fallingScale :: [Score Note]
fallingScale = [a',g'..a_]

fallingScaleSect :: Int -> [Score Note]
fallingScaleSect n = {-fmap (annotate (show n)) $-} take n $ fallingScale

mainSubject :: Score Note
mainSubject = stretch (1/6) $ asScore $ scat $ mapEvensOdds (accent . (^*2)) id $ concatMap fallingScaleSect [1..30]










mapEvensOdds :: (a -> b) -> (a -> b) -> [a] -> [b]
mapEvensOdds f g xs = let
    evens = fmap (xs !!) [0,2..]
    odds = fmap (xs !!) [1,3..]
    merge xs ys = concatMap (\(x,y) -> [x,y]) $ xs `zip` ys
    in take (length xs) $ map f evens `merge` map g odds


openAudacity :: Score Note -> IO ()    
openAudacity x = do
    void $ writeMidi "test.mid" $ x
    void $ system "timidity -Ow test.mid"
    void $ system "open -a Audacity test.wav"

openAudio :: Score Note -> IO ()    
openAudio x = do
    void $ writeMidi "test.mid" $ x
    void $ system "timidity -Ow test.mid"
    void $ system "open -a Audacity test.wav"

fixClefs :: Score Note -> Score Note
fixClefs = pcat . fmap (uncurry g) . extractParts'
    where
        g p x = clef (case defaultClef p of { 0 -> GClef; 1 -> CClef; 2 -> FClef } ) x

concurrently_ :: IO a -> IO b -> IO ()
concurrently_ = concurrentlyWith (\x y -> ())

concurrentlyWith :: (a -> b -> c) -> IO a -> IO b -> IO c
concurrentlyWith f x y = uncurry f <$> x `concurrently` y
