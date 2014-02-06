
{-# LANGUAGE
    OverloadedStrings,
    NoMonomorphismRestriction #-}

import Music.Prelude.Standard hiding (open, play, openAndPlay)
import Control.Concurrent.Async
import Control.Applicative
import Data.AffineSpace.Relative
import System.Process (system)

{-    
    String quartet
    Hommage a Henrik Strindberg
-}

-- Ensemble
[vl1, vl2]  = divide 2 (tutti violin)
vla         = tutti viola
vc          = tutti cello

score :: Score Note
score = mainCanon2

tremCanon = compress 4 $
    (delay 124 $ setPart vl1 $ subjs^*1)
        <>
    (delay 120 $ setPart vl2 $ subjs^*1)
        <>
    (delay 4 $ setPart vla $ subjs^*2)
        <>
    (delay 0 $ setPart vc  $ subjs^*2)
    where
        subjs = repeated [1..40] (\n -> palindrome $ rev $ subj n)
        subj n 
            | n < 8     = a_^*2  |> e^*1   |> a^*1
            | n < 16    = a_^*2  |> e^*1   |> a^*1   |> e^*1   |> a^*1
            | n < 24    = a_^*2  |> e^*0.5 |> a^*0.5 |> e^*0.5 |> a^*0.5
            | otherwise = e^*0.5 |> a^*0.5

mainCanon2 = palindrome mainCanon <> celloEntry

celloEntry = setPart vc e''^*(25*5/8)

mainCanon = timeSignature (time 6 8) $ asScore $ 
    (setPart vl1 $ harmonic 2 $ times 50 $ legato $ accentLast $ 
        octavesUp 2 $ scat [a_,e,a,cs',cs',a,e,a_]^/8) 

        <> 
    (setPart vl2 $ harmonic 2 $ times 50 $ legato $ accentLast $ 
        octavesUp 2 $ scat [d,g,b,b,g,d]^/8)^*(3/2)

        <> 
    (setPart vla $ harmonic 2 $ times 50 $ legato $ accentLast $ 
        octavesUp 2 $ scat [a,d,a,a,d,a]^/8)^*(3*2/2)

        <> 
    setPart vc a'^*(25*5/8)































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

palindrome x = rev x |> x




main :: IO ()
main = open score

play, open, openAndPlay :: Score Note -> IO ()   
tempo_ = 130
play x = openAudio $ stretch ((60*(8/3))/tempo_) $ fixClefs $ x
open x = openLilypond' Score $ fixClefs $ x
openAndPlay x = play x `concurrently_` open x

