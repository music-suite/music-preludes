
{-# LANGUAGE OverloadedStrings, FlexibleContexts, ConstraintKinds, TypeFamilies, RankNTypes #-}

import Music.Score (pitch) -- TODO
import qualified Music.Score
import Music.Prelude.Standard hiding (pitch, open, play, openAndPlay)
import Control.Concurrent.Async
import Control.Applicative
import Data.AffineSpace.Relative
import System.Process (system)
import qualified Data.Foldable
import Control.Lens hiding (Parts)
import Data.Default -- debug
import Math.OEIS

{-    
    Serial composition
    Using sequences from the OEIS
-}

main :: IO ()
main = openAndPlay score

ensemble :: [Part]
ensemble = (divide 4 (tutti violin)) <> (divide 2 (tutti viola)) <> (divide 2 (tutti cello)) <> [tutti bass]

type Scale = Integer -> Interval
scale :: Scale
scale n = case n `mod` 6 of
  0 -> _P1
  1 -> _M2
  2 -> _M3
  3 -> _A4
  4 -> _P5
  5 -> _M6

seq1 :: Score Integer
seq1 = scat $ take 500 $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2]
seq2 = scat $ take 500 $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2,1]
seq3 = scat $ take 500 $ fmap return $ fmap (`mod` 6) $ Data.Foldable.toList $ extendSequence [2,1,1,2,2,1,1]

score = (partNs 0 & up (m3^*2) & compress 6) <> (partNs 1 & up (m3^*1) & compress 5) <> (partNs 2 & compress 4)
  & pitch_ %~ normalize & compress 4

partNs n = part1 n <> part2 n <> part3 n
part1 n = asScore $ (part .~ (ensemble !! (0+3*n))) $ fmap (\x -> pitch' %~ (.+^ scale x) $ (c::Note)) $ seq1
part2 n = asScore $ (part .~ (ensemble !! (1+3*n))) $ fmap (\x -> pitch' %~ (.+^ scale x) $ (c::Note)) $ seq2
part3 n = asScore $ (part .~ (ensemble !! (2+3*n))) $ fmap (\x -> pitch' %~ (.+^ scale x) $ (c::Note)) $ seq3

-- instance Monoid Part where
--   mempty = def
-- instance Monoid p => Monad (PartT p) where
--   return x = PartT (mempty, x)











-- TODO remove Default 
parts :: (Default (Music.Score.Part a), Traversable t, HasPart a) => Traversal' (t a) (Music.Score.Part a) 
parts = traverse . part

part :: (Default (Music.Score.Part a), HasPart a) => Lens' a (Music.Score.Part a)
part = lens getPart (flip setPart)

part_ :: HasSetPitch a b => Setter a b (Music.Score.Pitch a) (Music.Score.Pitch b)
part_ = sets mapPitch

class Normal a where
    normalize :: a -> a
instance Normal Pitch where
    normalize = relative c (spell modal)



merge xs ys = concatMap (\(x,y) -> [x,y]) $ xs `zip` ys

mapEvensOdds :: (a -> b) -> (a -> b) -> [a] -> [b]
mapEvensOdds f g xs = let

    evens [] = []
    evens (x:xs) = x:odds xs

    odds [] = []
    odds (x:xs)  = evens xs

    in take (length xs) $ map f (evens xs) `merge` map g (odds xs)


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

play, open, openAndPlay :: Score Note -> IO ()   
tempo_ = 120
play x = openAudio $ stretch ((60*4)/tempo_) $ fixClefs $ x
open x = openLy' Score $ fixClefs $ x
openAndPlay x = play x `concurrently_` open x

