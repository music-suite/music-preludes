
import Music.Prelude.Basic
open = openLy . asScore

main = do
    return $ asScore score
    -- writeMidi "test.mid" score
    -- writeXml "test.xml" $ score^/4
    -- openXml score
    openLy score
    -- playMidiIO "Graphic MIDI" $ score^/10

{-    
    Pärt: Cantus in Memory of Benjamin Britten
    Copied from Abjad
-}

-- TODO tempo
-- TODO title
-- TODO 6/4

score = bell </> delay 6 strings


withTintin :: Pitch -> Score Note -> Score Note
withTintin p x = x <> tintin p x

tintin :: Pitch -> Score Note -> Score Note
tintin tonic = modifyPitch (relative tonic tintin')

-- | 
-- Given the melody voice return the tintinnabular voice. 
--
-- That is return the highest interval that is a member of the tonic triad in any octave,
-- which is less than the given interval 
--
tintin' :: Interval -> Interval
tintin' melInterval 
    | isNegative melInterval = error "tintin: Negative interval"
    | otherwise = last $ takeWhile (< melInterval) $ tnotes
    where


tnotes = concat $ iterate (fmap (+ _P8)) triad
    

triad :: [Interval]
triad = [_P1,m3,_P5]


bell :: Score Note
bell = let
    cue = stretchTo 1 (rest |> a) 
    in removeRests $ times 4 $ scat [times 3 $ scat [cue,rest], rest^*2]

fallingScale = [a',g'..a_]
fallingScaleSect n = {-fmap (annotate (show n)) $-} take n $ fallingScale
mainSubject = stretch (1/6) $ asScore $ scat $ mapEvensOdds (^*2) id $ concatMap fallingScaleSect [1..10]


mapEvensOdds :: (b -> a) -> (b -> a) -> [b] -> [a]
mapEvensOdds f g xs = let
    evens = fmap (xs !!) [0,2..]
    odds = fmap (xs !!) [1,3..]
    merge xs ys = concatMap (\(x,y) -> [x,y]) $ xs `zip` ys
    in take (length xs) $ map f evens `merge` map g odds

-- subj = scat [a, a,g, a,g,f, a,g,f,e, a,g,f,e,d, a,g,f,e,d,c, a,g,f,e,d,f,b, a,g,f,e,d,c,b,a]
-- subj = scat [annotate "1" a, a,g, a,g,f, a,g,f,e, annotate "2" a,g,f,e,d, a,g,f,e,d,c, a,g,f,e,d,f,b, a,g,f,e,d,c,b,a]

strings :: Score Note
strings = let
    vln1 = octavesUp 1 $ showAnnotations $ delay (1/2) $ withTintin a_ mainSubject
    vln2 = octavesDown 1 $ stretch 2 vln1
    vla  = setClef CClef $ octavesDown 2 $ stretch 4 vln1
    vc   = setClef FClef $ octavesDown 3 $ stretch 8 vln1
    db   = setClef FClef $ octavesDown 4 $ stretch 16 vln1
    in vln1 </> vln2 </> vla </> vc </> db