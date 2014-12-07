
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where
import Music.Time.Internal.Util (rotate)
import Music.Prelude
import qualified Data.Ratio

-- Get (bar,beat)
-- 400 bars in 60 BPM
-- 20 seconds in bar 401
-- 45 bars (to 446)
-- 30 seconds in bar 447       

-- notePos :: Int -> (Int,Int)
-- notePos t |                 t < 1600  =  ( t `div` 4 + 1 , t `mod` 4   + 1)
--           | 1600    <= t && t < 1620  =  ( 401           , t - (400*4)  + 1)
--           | 400*4+20 <= t && t < 445*4+20  =  ( (t-20) `div` 4 + 2, (t-20) `mod` 4 + 1 )
--           | otherwise                     =  447

main = {-openMusicXml-}open noteScore

noteScore :: Score StandardNote
noteScore = compress 4 $ {-addInstrChange $-}
      mempty

    -- * Part 1 (first canon and col legno)
    ||> (colLegno1  </> delay (4*3) colLegno1) 
    -- ||> (canon_I <> (delay (4*5) $ moveToPart vl2 $ canon_I))     -- A
    ||> (colLegno2  </> delay (4*3) colLegno2)                  -- B




    -- 
    -- -- * Part 2 (canon_II and surrounding)
    -- -- C
    -- ||> (level _p $ bar^*30
    --         <> delay 0      (moveToPart vc2 g_^*(4*13))
    --         <> delay (4*15) (moveToPart vc1 a_^*(4*13))
    --         )
    -- -- -- D, E
    -- ||> canon_II
    -- ||> (bar^*15 <> moveToPart vl2 (rev canon_II))
    -- -- -- F
    -- ||> (level _p $ bar^*30
    --         <> delay 0      (moveToPart vc2 bb_^*(4*15))
    --         <> delay (4*15) (moveToPart vc1 c  ^*(4*15))
    --         )
    -- ||> (canon_III <> (delay (4*30) $ moveToPart vl2 $ canon_III))     -- A
    -- 
    -- 
    -- -- * Part 3 (development to canon_IV)
    -- -- I
    -- ||> (mempty
    --         <> delay 0      (moveToPart vl1  f'  ^*(4*15))     
    --         <> delay (4*15) (moveToPart vl2  f'  ^*(4*15)) 
    --         )
    -- -- K
    -- ||> bar^*1
    -- ||> canon_IV
    -- 
    -- ||> rest^*20
    -- 
    -- -- * Part 4 (jete)
    -- -- FIXME sync back to score
    -- ||> mconcat [
    --         delay 0 $ level ppp $ up (12*3) $ moveToPart vl2  $ d_^*(4*30),
    --         delay (4*10) (level _p $ jete1 </> delay (12*8) jete1)
    --        ]
    -- ||> bar^*2
    -- ||> c'^*4 -- mark ending!


(||>) = (|>)


--------------------------------------------------------------------------------

colLegno1 :: Score StandardNote
colLegno1 = {-staccato $ -} level (ppp {-`cresc` mp |> mp^*0.2-}) $ text "col legno battuto"  $
        (down _P8  $ delay 0 $ repTimes 7 $ removeRests $ ([4,4,4,5,4] `groupWith` g) |> rest^*6)
    </> (down _P8  $ delay 1 $ repTimes 7 $ removeRests $ ([4,4,5,4,5] `groupWith` g) |> rest^*6)
    </> (down _P15 $ delay 3 $ repTimes 7 $ removeRests $ ([4,5,4,5,4] `groupWith` g) |> rest^*6)
    </> (down _P15 $ delay 6 $ repTimes 7 $ removeRests $ ([3,3,5,3,5] `groupWith` g) |> rest^*6)

-- dur 45

colLegno2 :: Score StandardNote
colLegno2 = {-staccato $ -} level (mp) $ text "col legno battuto"  $
        (down _P8 $ delay 0 $ repTimes 4 $ removeRests $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down _P8 $ delay 1 $ repTimes 4 $ removeRests $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
    </> (down _P8 $ delay 3 $ repTimes 4 $ removeRests $ [4,5,4,5,4,4]  `groupWith` g |> rest^*6)
    </> (down _P8 $ delay 6 $ repTimes 4 $ removeRests $ [3,3,5,3,3]    `groupWith` g |> rest^*6)
-- 
colLegno2Alt :: Score StandardNote
colLegno2Alt = {-staccato $ -} level (mp) $ text "col legno battuto"  $
        (down _P8 $ delay 0 $ removeRests $ repWithIndex 4 $ \t -> [4,4,5,4,5,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down _P8 $ delay 1 $ removeRests $ repWithIndex 4 $ \t -> [4,4,5,4,5,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down _P15 $ delay 3 $ removeRests $ repWithIndex 4 $ \t -> [4,5,4,5,4,4]  `groupWith` g |> rest^*(1+4*t))
    </> (down _P15 $ delay 6 $ removeRests $ repWithIndex 4 $ \t -> [3,3,5,3,3]    `groupWith` g |> rest^*(1+4*t))

-- --------------------------------------------------------------------------------
-- 
makeJete :: Behavior Interval -> Bool -> Duration -> Score (Maybe StandardNote)
makeJete p v d = {-text "jeté" $-} pitches' %~ (.+^ p) $ removeRests $ g_ |> ((if v then cs else cs_){-^/2-}) {-|> rest^/2-} |> rest^*d

makeJetes :: [Behavior Interval] -> [Bool] -> [Duration] -> Score (Maybe StandardNote)
makeJetes ps vs ds = scat $ zipWith3 makeJete ps vs ds

jete1 :: Score StandardNote
jete1 = removeRests $ -- FIXME temporary fix w.r.t onset/padToBar
        (delay 3  $ up _P1   $ makeJetes (rotate 0 ps) (rotate 3 vs) (rotate 1 ds))
    </> (delay 5  $ up _P1   $ makeJetes (rotate 1 ps) (rotate 0 vs) (rotate 3 ds))^*(4/5)
    </> (delay 7  $ down _P8 $ makeJetes (rotate 2 ps) (rotate 1 vs) (rotate 2 ds))
    </> (delay 12 $ down _P8 $ makeJetes (rotate 3 ps) (rotate 2 vs) (rotate 0 ds))^*(4/5)
    where
        ps = take n $ cycle [_P1,d5,d5,_P1,d5,d5,_P1]
        vs = take n $ cycle [True,False,True,False,True,False,True,False]
        ds = take n $ cycle $ fmap (+ 4) [3,7,5,7,5,5,3,7,7,7,7,7,5,3,7,7,7,7,7,3,3,5]
        n  = 9
-- 
-- -- colLegno3 :: Score StandardNote
-- -- colLegno3 = (down _P8 $ delay 0 $ rep $ [4,4,5,4,5,4]  `groupWith` g |> rest^*6)
-- 
-- 
-- --------------------------------------------------------------------------------
-- 
makeCanon_I :: Rational -> Dynamic StandardNote -> Score StandardNote -> Score StandardNote -> Score StandardNote
makeCanon_I n dn subj1 subj2 =
        level dn (empty |> (a </> b </> c </> d))
    where
        a = (repTimes (floor $ 5*n/(4/3)) $ subj1 ^*(4/3))
        b = (repTimes (floor $ 5*n/1)     $ subj2 ^*1)
        c = (repTimes (floor $ 5*n/2)     $ subj1 ^*2)
        d = (repTimes (floor $ 5*n/3)     $ subj2 ^*3)
-- 
canon_I :: Score StandardNote
canon_I = text "ord" $ (^*2) $ makeCanon_I 1 {-dn-}mf subj1 subj2
    where
        subj1 = g_ |> a_^*(3/2) |> g_^*2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*3
        -- dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )

makeCanon_II :: Dynamic StandardNote -> Score StandardNote -> Score StandardNote -> Score StandardNote
makeCanon_II dn subj1 subj2 =
        level dn (rev $ a </> b </> c </> d)
    where
        a = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*(4/3))
        b = (repWithTime 5 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*1)
        c = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj1 ^*2)
        d = (repWithTime 2 $ \t -> {-up (round $ octave * t) $ -}subj2 ^*3)

repWithTime n f = scat $ fmap f [1..n]

canon_II :: Score StandardNote
canon_II = text "ord" $ (^*2) $ makeCanon_II {-dn-}mf subj1 subj2
    where
        subj1 = g_ |> d^*(3/2) |> c^/2 |> a_^/2 |> bb_^/2
        subj2 = f_^*3 |> bb_^*1 |> a_ |> d_^*3
        -- dn   = (repTimes 5 $ (pp `cresc` mf)^*3 |> (mf `dim` pp)^*3 )
-- 
-- makeCanon_III :: Double -> Score (Levels Double) -> Score StandardNote -> Score StandardNote -> Score StandardNote
-- makeCanon_III n dn subj1 subj2 =
--         level dn (rev (a </> b </> c </> d) |> (a </> b </> c </> d))
--     where
--         a = (repTimes (5*n/(4/3)) $ subj1 ^*(4/3))
--         b = (repTimes (5*n/1)     $ subj2 ^*1)
--         c = (repTimes (5*n/2)     $ subj1 ^*2)
--         d = (repTimes (5*n/3)     $ subj2 ^*3)
-- 
-- canon_III :: Score StandardNote
-- canon_III = text "ord" $ makeCanon_III 1.6 dn subj1 subj2
--     where
--         subj1 = g^*2 |> d |> eb^*(3/2) |> c^*2 |> d^*2
--         subj2 = f_^*3 |> bb_^*1 |> a_ |> g_^*2 |> d^*3 |> c^*1
--         dn   = (repTimes 5 $ (mf `cresc` _f)^*3 |> (_f `dim` mf)^*3 )
-- 
makeCanon_IV :: Bool -> Score StandardNote -> Score StandardNote -> Score StandardNote -> Score StandardNote
makeCanon_IV flip subj1 subj2 bass = if flip then lower </> upper else upper </> lower
    where
        upper = (repWithTime (10/(4/5)) $ \t -> reg "Vl1" t   $ subj1 ^* (4/5) )
            </> (repWithTime (12/(2/3)) $ \t -> reg "Vla1" t  $ subj1 ^* (2/3) )
            </> (repWithTime (15/ 1   ) $ \t -> reg "Vc1" t   $ subj1 ^* 1     )
            </> (repWithTime (18/ 2   ) $ \t -> reg "Db2" t   $ bass ^* 1    )

        lower = (repWithTime (10/(2/3)) $ \t -> reg "Vl2" t   $ subj2 ^* (2/3) )
            </> (repWithTime (12/ 1   ) $ \t -> reg "Vla2" t  $ subj2 ^* 1     )
            </> (repWithTime (15/(3/2)) $ \t -> reg "Vc2" t   $ subj2 ^* (3/2) )
            </> (repWithTime (18/ 2   ) $ \t -> reg "Db2" t   $ bass ^* 1    )

        reg "Vl1"  t | t < 0.3 = up   (_P8 + _P5) | t < 0.6 = up _P8       | t >= 0.6 = up _P5
        reg "Vl2"  t | t < 0.4 = up   _P8           | t < 0.7 = up _P5        | t >= 0.7 = up _P5
        reg "Vla1" t | t < 0.4 = up   _P5            | t < 0.7 = up _P5        | t >= 0.7 = up _P1
        reg "Vla2" t | t < 0.4 = up   _P1           | t < 0.7 = up _P5        | t >= 0.7 = up _P1
        reg "Vc1"  t | t < 0.4 = down _P8           | t < 0.7 = down _P8     | t >= 0.7 = down _P4
        reg "Vc2"  t | t < 0.4 = down _P8           | t < 0.7 = down _P8     | t >= 0.7 = down _P4

        reg "Db1"  t | t < 0.4 = down (_P8^*1)       | t < 0.7 = down (_P8^*1) | t >= 0.7 = down (_P8^*1)
        reg "Db2"  t | t < 0.4 = down (_P8^*2)       | t < 0.7 = down (_P8^*1) | t >= 0.7 = down (_P8^*1)


canon_IV :: Score StandardNote
canon_IV = text "ord" $ {-c^*padC |> firstC |>-} secondC
    where
        firstC  = {-level dn1 $-} rev $ makeCanon_IV False subj1 subj2 bass
        secondC = {-level dn2 $-}       makeCanon_IV True subj1 subj2 bass
        padC    = fromIntegral $ 4 - Data.Ratio.numerator (realToFrac $ _duration firstC) `mod` 4
        -- dn1     = (repTimes 10 $ (mf `cresc` _f)^*5 |> (_f `dim` mf)^*5)
        -- dn2     = (repTimes 10 $ (_f `cresc` ff)^*5 |> (ff `dim` _f)^*5)

        subj1 = down _M2 $ (d^*3 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
        subj2 = down _M2 $ (d^*2 |> a |> g^*2 |> c' |> b |> c' |> b |> g |> a^*3)
        bass  = scat [d,a] |> g^*2 |> scat [c,d,a] |> g^*2
        -- bass  = scat [d,g] |> a^*2 |> scat [c,g,d] |> a^*2


instance Reversible (Score a) where
  rev = stretchRelativeMidpoint (-1)

cresc = const
dim   = const
repTimes = times

-- groupWith :: [Int] -> a -> a
groupWith xs p = scat $ fmap (\n -> group n p) xs

group n x      = times n x^/(fromIntegral n)
repWithIndex n f = scat $ fmap f [1..n]

moveToPart p x = parts' .~ p $ x

[vl1, vl2] = divide 2 $ tutti violin


_solo :: Lens' Part Solo
_solo f (Part s i u) = fmap (\s -> Part s i u) $ f s

_subpart :: Lens' Part Subpart
_subpart f (Part s i u) = fmap (\u -> Part s i u) $ f u

_instrument :: Lens' Part Instrument
_instrument f (Part s i u) = fmap (\i -> Part s i u) $ f i

-- Not perfect but works for many cases
ucat xs = if allDistinct ps 
    then mconcat xs 
    else mconcat $ zipWith (set parts') (divide (length xs) p) xs
  where
    ps = concatMap (toListOf parts') xs
    p  = foldr1 smallestPart ps



