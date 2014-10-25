-- -fno-warn-typed-holes
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Music.Prelude hiding ((</>))
import qualified Music.Score




-- mn'  = "(a (b c)) (> gs (a b)) ((c e d c) %-1d) %-2d"
dot x y = stretchTo 1 (stretch 3 x <> stretch 1 y)
mn'' = (stretchTo 1.mconcat)[(stretchTo 1.mconcat)[a,(stretchTo 1.mconcat)[b,c]],(stretchTo 1.mconcat)[gs `dot` (stretchTo 1.mconcat)[a,b]]]



mn = scat [a^*2,b,c',d',gs^*2,scat[a,b]^/2]^/4
cn = (\x -> x |> downDiatonic c 1 x |> (downDiatonic c 2 x)) $ scat [c',e',d',c']^/8

s  = mn |> cn
m  = set (pitches'.mapped.rel c._alteration) 0 $ upDiatonic c 2 s


-- Expositions
ex1 = delay 2 (up _P5 s)   </> s
ex2 = delay 2 (down _P4 s) </> s
ex3 = delay 2 (up _P5 m)   </> m
ex4 = delay 2 (down _P4 m) </> m

main = open $ over parts' id $ asScore ex1























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
(<<>) = (</>)

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

upDiatonic o n = over (pitches'.mapped) (upDiatonicP o n)
downDiatonic o n = over (pitches'.mapped) (downDiatonicP o n)

rel o = iso (\p -> p .-. o) (\v -> o .+^ v)


