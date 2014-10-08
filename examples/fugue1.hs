
{-# LANGUAGE TypeFamilies #-}

import Music.Prelude hiding ((</>))
import qualified Music.Score

s = set parts' violins $ scat [a^*2,b,c',gs^*3,scat[a,b]^/2]^/4


ex1 = delay 2 (up _P5 s) </> s



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

-- TODO check parts and divide if equal
