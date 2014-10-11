-- -fno-warn-typed-holes
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Music.Prelude hiding ((</>))
import qualified Music.Score

mn = set parts' violins $ scat [a^*2,b,c',gs^*3,scat[a,b]^/2]^/4
cn = (\x -> x |> downDiatonic c 1 x |> (downDiatonic c 2 x)) $ scat [c',e',d',c']^/8
s = mn |> cn

ex1 = delay 2 (up _P5 s) </> s

main = open $ asScore ex1























-- |
-- Parallel composition preserving parts.
--
-- If the part of a and b differ, compose using (<>).
-- If the part of a and b equal, the contents of a and b are placed in section 1 2 and section 2 2 respectively.
--
-- TODO handle subsections correctly. I.e. the parts must not only be distinct but noone must be a superset of the other
-- (what's the terminology for this?).
-- For an unexpected result, see
-- >>> foldl (</>) mempty [c,d,e,f,g]
--
(</>) :: (HasParts' a, Semigroup a, Music.Score.Part a ~ Part) => a -> a -> a
a </> b = set parts' pa a <> set parts' pb b
  where
    (pa, pb) = case (a^?parts, b^?parts) of
        (Nothing,Nothing) -> (mempty, mempty)
        (Just x, Nothing) -> (x, mempty)
        (Nothing,Just x)  -> (mempty, x)
        (Just x, Just y)  -> if x /= y then (x, y) else divide2 x
    divide2 a = let [x,y] = divide 2 a in (x,y)
    
    -- if equal
    -- [pa',pb'] = divide 2 pa

data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)

expectedQualityType :: HasNumber a => a -> QualityType
expectedQualityType x = if ((abs (number x) - 1) `mod` 7) + 1 `elem` [1,4,5]
  then PerfectType else MajorMinorType

qualityTypes :: Quality -> [QualityType]
qualityTypes Perfect = [PerfectType]
qualityTypes Major   = [MajorMinorType]
qualityTypes Minor   = [MajorMinorType]
qualityTypes _       = [PerfectType, MajorMinorType]

qualityToDiff :: QualityType -> Quality -> Int
qualityToDiff MajorMinorType (Augmented n)  = 0 + n
qualityToDiff MajorMinorType Major = 0
qualityToDiff MajorMinorType Minor          = (-1)
qualityToDiff MajorMinorType (Diminished n) = (-1) - n
qualityToDiff PerfectType (Augmented n)  = 0 + n
qualityToDiff PerfectType Perfect        = 0
qualityToDiff PerfectType (Diminished n) = 0 - n
qualityToDiff qt q = error $ "qualityToDiff: Unknown interval expression (" ++ show qt ++ ", " ++ show q ++ ")"

toMajorMinorType Perfect = Major
toPerfectType    Major   = Perfect
toPerfectType    Minor   = (Diminished 1)
-- toPerfectType    x = x




p1 = scat $ take 5 $ iterate (times 3 . compress 2) $ scat [c,g,f]
p2 = scat $ take 5 $ iterate (times 3 . compress 2) $ scat [c,g,fs,f]
p3 = scat $ take 4 $ iterate (times 3 . compress 2) $ scat [c,ab,g,fs,f]
p4 = scat $ take 4 $ iterate (times 3 . compress 2) $ scat [c,bb,a,ab,g]

{-
>>> let x = scat [colorRed c,d,e] in (x |> scat (fmap (\n -> text (show n) $ upDiatonic c n x) [0..10]))

-- TODO this one still crashes
>>> let x = scat [colorRed c,d,e] in (x |> scat (fmap (\n -> text (show n) $ upDiatonic c n x) [0..10]))


-}


upDiatonic :: (HasPitches' t, Functor f, Music.Score.Pitch t ~ f Pitch) => Pitch -> DiatonicSteps -> t -> t
upDiatonic o n = over pitches' (fmap $ upDiatonicP o n)

downDiatonic :: (HasPitches' t, Functor f, Music.Score.Pitch t ~ f Pitch) => Pitch -> DiatonicSteps -> t -> t
downDiatonic o n = over pitches' (fmap $ downDiatonicP o n)

invertPitchesDiatonically :: (HasPitches' t, Functor f, Music.Score.Pitch t ~ f Pitch) => Pitch -> t -> t
invertPitchesDiatonically o = over pitches' (fmap $ invertPitchesDiatonicallyP o)



-- TODO workaround: transpose down by transposing upward and octave-transposing

upDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
upDiatonicP origin n = relative origin $ (_steps +~ n) . (_quality %~ if n >= 0 then id else invertQuality)

downDiatonicP :: Pitch -> DiatonicSteps -> Pitch -> Pitch
downDiatonicP origin n = relative origin $ (_steps -~ n) .  (_quality %~ if n < 0 then id else invertQuality)

-- TODO mixing up reflection point and tonic?
-- Basically, we always invert around c, adding relative only moves the tonic and inverts around c
invertPitchesDiatonicallyP :: Pitch -> Pitch -> Pitch
invertPitchesDiatonicallyP origin = relative origin $ ((_steps %~ negate) . (_quality %~ invertQuality))

interval :: Iso' (Quality, Number) Interval
interval = iso (uncurry mkIntervalNice) (\x -> (quality x, number x))

{-|
>>> m3 & _number %~ pred
m2
>>> m3 & _number %~ succ
d4
>>> _M3 & _number %~ succ
_P4


>>> m3 & _number +~ 1
d4
>>> m3 & _number +~ 2
d5
>>> m3 & _number +~ 3
m6
>>> m3 & _number +~ 4


>>> m3 & _quality .~ Minor
m3
>>> _P5 & _quality .~ Minor
d5
>>> (-d5) & _quality %~ diminish


TODO only obeys lens laws up to quality normalization

>>> _P5 & _quality .~ Minor
d5
>>> _P5 & _quality .~ (Diminished 1)
d5

-}

newtype DiatonicSteps = DiatonicSteps { getDiatonicSteps :: Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

diatonicSteps :: Iso' Number DiatonicSteps
diatonicSteps = iso n2d d2n
  where
    n2d n | n > 0  = fromIntegral (n - 1)
    n2d n | n == 0 = error "diatonicSteps: Invalid number 0"
    n2d n | n < 0  = fromIntegral (n + 1)

    d2n n | n >= 0 = fromIntegral (n + 1)
    d2n n | n <  0 = fromIntegral (n - 1)

_quality :: Lens' Interval Quality
_quality = from interval . _1

_number :: Lens' Interval Number 
_number = from interval . _2

_steps :: Lens' Interval DiatonicSteps
_steps = _number . diatonicSteps

-- TODO be "nice"
mkIntervalNice q n
  | n == 0 = error "mkIntervalNice: Invalid number 0"
  | n < 0 = negate $ mkIntervalNice q (abs n)
  | expectedQualityType n `elem` qualityTypes q = mkInterval q n 
  | expectedQualityType n == MajorMinorType     = mkInterval (toMajorMinorType q) n
  | expectedQualityType n == PerfectType        = mkInterval (toPerfectType q) n
