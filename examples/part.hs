
{-# LANGUAGE
    OverloadedStrings #-}

import Music.Prelude.Standard
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
strings = let
    vln1 = setClef GClef $ setPart (ensemble !! 1) $ octavesUp   1 $ cue
    vln2 = setClef GClef $ setPart (ensemble !! 2) $ octavesDown 0 $ stretch 2 cue
    vla  = setClef CClef $ setPart (ensemble !! 3) $ octavesDown 1 $ stretch 4 cue
    vc   = setClef FClef $ setPart (ensemble !! 4) $ octavesDown 2 $ stretch 8 cue
    db   = setClef FClef $ setPart (ensemble !! 5) $ octavesDown 3 $ stretch 16 cue
    in vln1 <> vln2 <> vla <> vc <> db
    where
        cue = delay (1/2) $ withTintin (octavesDown 4 a) mainSubject

fallingScale :: [Score Note]
fallingScale = [a',g'..a_]

fallingScaleSect :: Int -> [Score Note]
fallingScaleSect n = {-fmap (annotate (show n)) $-} take n $ fallingScale

mainSubject :: Score Note
mainSubject = stretch (1/6) $ asScore $ scat $ mapEvensOdds (accent . (^*2)) id $ concatMap fallingScaleSect [1..30]

mapEvensOdds :: (b -> a) -> (b -> a) -> [b] -> [a]
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

instance HasMidiProgram Part where
    getMidiChannel = defaultMidiChannel
    getMidiProgram = fixStrings . defaultMidiProgram
        where 
            fixStrings x = case x of
                40 -> 48
                41 -> 48
                42 -> 48
                x  -> x

