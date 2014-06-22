
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
 
import Music.Prelude
import Language.Scheme.Core
import Language.Scheme.Types
import Language.Scheme.Variables
import Language.Scheme.Parser
import Data.Traversable
import System.IO.Unsafe
import Data.IORef
import Data.Maybe
import Data.Either
import qualified Music.Score as Score
import qualified Data.Map as Map
-- import qualified Data.List
-- import System.Random

lispApi = [
  ("c", toLisp (c :: Score Integer)),
  ("d", toLisp (d :: Score Integer)),
  ("e", toLisp (e :: Score Integer)),
  ("f", toLisp (f :: Score Integer)),
  ("g", toLisp (g :: Score Integer)),
  ("a", toLisp (a :: Score Integer)),
  ("b", toLisp (b :: Score Integer)),

  ("c#", toLisp (cs :: Score Integer)),
  ("d#", toLisp (ds :: Score Integer)),
  ("e#", toLisp (es :: Score Integer)),
  ("f#", toLisp (fs :: Score Integer)),
  ("g#", toLisp (gs :: Score Integer)),
  ("a#", toLisp (as :: Score Integer)),
  ("b#", toLisp (bs :: Score Integer)),

  ("cb", toLisp (cb :: Score Integer)),
  ("db", toLisp (db :: Score Integer)),
  ("eb", toLisp (eb :: Score Integer)),
  ("fb", toLisp (fb :: Score Integer)),
  ("gb", toLisp (gb :: Score Integer)),
  ("ab", toLisp (ab :: Score Integer)),
  ("bb", toLisp (bb :: Score Integer)),

  ("s01", toLisp (0 <-> 1)),
  ("pass",    CustFunc $ lift1 (id :: Span -> Span)),
  
  ("stretch", CustFunc $ lift2 (stretch :: Duration -> Score Integer -> Score Integer)),
  ("move",    CustFunc $ lift2 (delay   :: Duration -> Score Integer -> Score Integer)),
  ("|>",      CustFunc $ lift2 ((|>) :: Score Integer -> Score Integer -> Score Integer)),
  ("<>",      CustFunc $ lift2 ((|>) :: Score Integer -> Score Integer -> Score Integer)),

  ("first-argument",  CustFunc $ \(x : _) -> return x),
  ("second-argument", CustFunc $ \(_ : x : _) -> return x)
  ]



main = do
  stdEnv <- r5rsEnv
  env <- extendEnv stdEnv (map (over _1 (varNamespace,)) $ lispApi)
  code <- readFile "examples/test1.scm"
  res <- evalLisp' env $ fromRight $ readExpr code
  let sc = (\x -> x :: Score Integer) $ fromJust $ fromRight $ fmap fromLisp $ res
  -- printScore sc
  openLilypond $ fmap (\x -> PartT(mempty::Part,TieT(mempty,ArticulationT(mempty::(Sum Double,Sum Double),DynamicT(mempty::Sum Double,[x]))))) $ sc
  return ()

printScore = mapM_ print . view notes
fromRight (Right x) = x  


class HasLisp a where
  _unlisp :: Prism' LispVal a

toLisp :: HasLisp a => a -> LispVal
toLisp = view (re _unlisp)

fromLisp :: HasLisp a => LispVal -> Maybe a
fromLisp = preview _unlisp

instance HasLisp Bool where
  _unlisp = prism' Bool $ \x -> case x of
    Bool x -> Just x
    _      -> Nothing

instance HasLisp Integer where
  _unlisp = prism' Number $ \x -> case x of
    Number x -> Just x
    _        -> Nothing

instance HasLisp Double where
  _unlisp = prism' Float $ \x -> case x of
    Float x -> Just x
    _       -> Nothing

instance HasLisp Rational where
  _unlisp = prism' Rational $ \x -> case x of
    Rational x -> Just x
    _       -> Nothing

instance HasLisp Duration where
  _unlisp = _unlisp . rationalFrac

instance HasLisp Time where
  _unlisp = _unlisp . rationalFrac

instance HasLisp Span where
  _unlisp = _unlisp . from delta

instance HasLisp a => HasLisp (Score.Note a) where
  _unlisp = _unlisp . note

instance HasLisp a => HasLisp (Score a) where
  _unlisp = _unlisp . from unsafeNotes

instance (HasLisp a, HasLisp b) => HasLisp (a, b) where
  _unlisp = prism' (lcons . over _1 toLisp . over _2 toLisp) $ \xs -> case xs of
      DottedList [x] y -> case (fromLisp x, fromLisp y) of
        (Just x, Just y) -> Just (x, y)
        _                -> Nothing
      _                -> Nothing                          
    where
      lcons (x, y) = DottedList [x] y

instance HasLisp a => HasLisp [a] where
  _unlisp = prism' (List . map toLisp) $ \xs -> case xs of
    List xs -> sequenceA $ map fromLisp $ xs
    _       -> Nothing

-- instance HasLisp Double where
  -- _unlisp = _unlisp . iso fromInteger (toInteger.round)

type LispFunc = [LispVal] -> IOThrowsError LispVal

lift1 :: (HasLisp a, HasLisp b) => (a -> b) -> LispFunc
lift1 f [a1] = case (fromLisp a1) of
  (Just a1) -> return $ toLisp $ f a1
  _         -> fail $ "Type error: got " ++ show a1
lift1 f _ = fail "Wrong number of args"

lift2 :: (HasLisp a, HasLisp b, HasLisp c) => (a -> b -> c) -> LispFunc
lift2 f [a1, a2] = case (fromLisp a1, fromLisp a2) of
  (Just a1, Just a2) -> return $ toLisp $ f a1 a2
  _                  -> fail "Type error"
lift2 f _ = fail "Wrong number of args"
  
-- lift2 :: (a -> b -> c) -> LispFunc


{-
makeEnv :: [(String, LispFunc)] -> Env -> Env
makeEnv bindings = composed $ map (uncurry extendEnv2 . fmap CustFunc) $ bindings
  where
composed = foldr (.) id

extendEnv2 :: String -> LispVal -> Env -> Env
extendEnv2 k v p = Environment (Just p) (retR $ toMap ("_" ++ k) (retR v)) (retR mempty) 
  where
    toMap k v = Map.insert k v mempty
      
    retR :: a -> IORef a
    retR = unsafePerformIO . newIORef
-}


doubleFrac = iso fromDouble toDouble
    where
      toDouble :: Real a => a -> Double
      toDouble = realToFrac

      fromDouble :: Fractional a => Double -> a
      fromDouble = realToFrac

rationalFrac = iso fromRational toRational

