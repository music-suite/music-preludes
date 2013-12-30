
{-# LANGUAGE
    OverloadedStrings #-}

import Music.Prelude.Standard hiding (open, play, openAndPlay)
import Control.Concurrent.Async
import Control.Applicative
import Data.AffineSpace.Relative
import System.Process (system)

{-    
    Mozart - Ave Verum
    
    Transcribed from autograph, see
        http://imslp.org/wiki/Ave_verum_corpus,_K.618_(Mozart,_Wolfgang_Amadeus)

    Divided as follows (including preceding accompaniement):

        stanza1:    Ave verum corpus natum de Maria virgine
        stanza2:    Vere passum immolatum in cruce pro homoni
        stanza3:    Cujus latus perforatum unda fluxit et sanguine
        stanza4:    Esto nobis praegustatum in mortis examine
-}


-- Vocal parts
[vl1, vl2]  = divide 2 (tutti violin)
vla         = tutti viola
vc          = tutti cello

-- Instruments
[sop, alt]  = divide 2 (tutti violin)
ten         = tutti viola
bs          = tutti cello
bc          = tutti bass


score = compress 4 $ tempo (metronome (1/4) 30) $ {-delay (4*2) $ -} 
    stanza1_instr </> stanza1_voc

-- Rhythm helper functions
lss l s1 s2     = l^*2 |> s1 |> s2
ssl s1 s2 l     = s1 |> s2 |> l^*2
s4 s1 s2 s3 s4  = s1 |> s2 |> s3 |> s4
sl s l          = s |> l^*3
ls l s          = l^*3 |> s
fit2 x y        = (x |> y)^/2
l4 l            = l^*4
ll l1 l2        = (l1 |> l2)^*2

-- Stanza 1
stanza1_voc = stanza1_sop </> stanza1_alto </> stanza1_ten </> stanza1_bass
stanza1_sop = asScore $ delay 8 $ mempty
    |> lss a d' fs |> ssl a gs g   |> s4 g b a g           |> ssl g fs fs
    |> ls e e      |> s4 fs fs g g |> lss g (fit2 fs e) fs |> l4 e
stanza1_alto = asScore $ delay 8 $ mempty
    |> ll fs fs    |> ll e e       |> s4 e g fs e          |> ssl e d d   
    |> ls cs cs    |> s4 d d e e   |> lss e (fit2 d cs) d  |> l4 cs
stanza1_ten = asScore $ delay 8 $ octavesDown 1 $ mempty
    |> ll a  a     |> ll b b       |> ls a   a             |> ll a a   
    |> ls e  e     |> s4 a a b b   |> ls a             a   |> l4 e
stanza1_bass = asScore $ delay 8 $ octavesDown 1 $ mempty
    |> ll d  d     |> ll d d       |> ls cs  cs            |> ll d d   
    |> ls a  a     |> s4 d d cs cs |> ls d             d   |> l4 a_

stanza1_instr = stanza1_vl1 </> stanza1_vl2 </> stanza1_vla </> stanza1_bc
stanza1_vl1 = asScore $ mempty
    |> s4 d a_ d e |> s4 fs d fs g
    |> lss a d' fs |> ssl a gs g   |> s4 g b a g           |> ssl g fs fs
    |> ls e e      |> s4 fs fs g g |> lss g (fit2 fs e) fs |> l4 e
stanza1_vl2 = asScore $ mempty
    |> s4 d a_ d e |> s4 fs d fs g
    |> ll fs fs    |> ll e e       |> s4 e g fs e          |> ssl e d d   
    |> ls cs cs    |> s4 d d e e   |> lss e (fit2 d cs) d  |> l4 cs
stanza1_vla = asScore $ octavesDown 1 $ mempty
    |> s4 d a_ d e |> s4 fs d fs g
    |> ll a  a     |> ll b b       |> ls a   a             |> ll a a   
    |> ls e  e     |> s4 a a b b   |> ls a             a   |> l4 e
stanza1_bc = asScore $ octavesDown 1 $ mempty
    |> s4 d a_ d e |> s4 fs d fs g
    |> ll d  d     |> ll d d       |> ls cs  cs            |> ll d d   
    |> ls a  a     |> s4 d d cs cs |> ls d             d   |> l4 a_

-- Stanza 1




























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
tempo_ = 80
play x = openAudio $ stretch ((60*(8/3))/tempo_) $ fixClefs $ x
open x = openLy' Score $ fixClefs $ x
openAndPlay x = play x `concurrently_` open x

