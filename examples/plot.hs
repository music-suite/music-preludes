
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Graphics.EasyPlot 

import Music.Prelude.Basic
import Music.Time.Reactive 
import Music.Time.Behavior

---
import Prelude hiding (null)
import Data.AffineSpace
import Data.VectorSpace
import Data.Sequence hiding (reverse)
---

durToPitch :: Duration -> Pitch
durToPitch = fromInteger . round

durToInterval :: Duration -> Interval
durToInterval = fromInteger . round

sc :: Score (Behavior Pitch)
sc = times 240 $ note (varying $ const 0)

changePitch :: Behavior Pitch -> Behavior Pitch
changePitch = liftA2 (^+.) $ 
    switchB 24 (pure _P1) 
        (switchB 48 sine sine2)
    where
        sine  = varying (durToInterval.(*12).sinR.(* (tauR/12)))
        sine2 = delay 48 $ varying (durToInterval.(*13).sinR.(* (tauR/13)))



-- main = do
    -- openLilypond $ sc2^/12
    -- playMidiIO "" $ sc2^/12

-- sc2 = fmap (? 0) $ __mapPitch changePitch sc
        

tau = 2*pi
tauR = realToFrac tau
sinR :: (Real a, Fractional b) => a -> b
sinR = realToFrac . sin . realToFrac
(^+.) = flip (.+^)


plotR :: (Num a, Show a) => Reactive a -> IO ()
plotR r = do
    plot X11 $ (\t -> r ? realToFrac t)
    return ()

plotRs :: (Num a, Show a) => [Reactive a] -> IO ()
plotRs rs = do
    plot X11 $ map (\r -> (\t -> r ? realToFrac t)) rs
    return ()

plotB :: (Num a, Show a) => Behavior a -> IO ()
plotB r = do
    plot X11 $ (\t -> r ? realToFrac t)
    return ()

plotBs :: (Num a, Show a) => [Behavior a] -> IO ()
plotBs rs = do
    plot X11 $ map (\r -> (\t -> r ? realToFrac t)) rs
    return ()


-- main = 
--     plotRs $ [delay (-0.5) r1, stretch 1.1 $ delay (-0.5) r1]
-- r1 = switch (-3) (pure (-3)) (switch 3 (activate ((0 <-> 1) =: (pure 1)) (pure 0)) (pure 3))














-- From http://alexis.vallet.free.fr/?p=412
bezier, bezier' :: (AffineSpace p, VectorSpace (Diff p)) => 
                   [p] -> Scalar (Diff p) -> p

bezier' [p] _ = p
bezier' polygon t = 
    let poly0 = reverse . tail $ reverse polygon
        poly1 = tail polygon in
    alerp (bezier' poly0 t) (bezier' poly1 t) t

bezier polygon t =
    bezierSeq (fromList polygon) t
 
bezierSeq :: (AffineSpace p, VectorSpace (Diff p)) => 
             Seq p -> Scalar (Diff p) -> p
bezierSeq polygon t =
    let poly0 :> _ = viewr polygon
        p :< poly1 = viewl polygon in
    if null poly1
    then p
    else alerp (bezierSeq poly0 t) (bezierSeq poly1 t) t

examplePolygon :: [(Double, Double)]
examplePolygon = [(0, 0), (1, 1), (2, 0)]

