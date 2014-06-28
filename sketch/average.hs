
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

import Data.Semigroup
-- import Data.NumInstances
-- import Data.Semigroup.Instances

newtype Average a = Average (Sum Int, Sum a)
  deriving (Show)

instance Num a => Num (Sum a) where
  Sum a + Sum b = Sum (a + b)
  Sum a * Sum b = Sum (a * b)
  negate (Sum a) = Sum (negate a)
  signum (Sum a) = Sum (signum a)
  abs (Sum a) = Sum (abs a)
  fromInteger n = Sum (fromInteger n)
  
instance (Monoid a, Num a, Num b) => Num (a, b) where
  (a1,b1) + (a2,b2) = (a1, b1+b2)
  (a1,b1) * (a2,b2) = (a1, b1*b2)
  negate (a1, b1) = (a1, negate b1)
  signum (a1, b1) = (a1, signum b1)
  abs (a1, b1)    = (a1, abs b1)
  fromInteger n   = (1, fromInteger n)
  
deriving instance Num a => Num (Average a)
deriving instance Num a => Semigroup (Average a)
deriving instance Num a => Monoid (Average a)

getAverage :: Fractional a => Average a -> a
getAverage (Average (Sum n, Sum a)) = a / fromIntegral n




-- | Mean of sample. Samples of Double,Float and bui;t-in integral
--   types are supported
--
-- Numeric stability of 'mappend' is not proven.
data Mean a = Mean { calcMean      :: a -- ^ Current mean
                 , calcCountMean :: Int    -- ^ Number of entries
                 }
            deriving Show

instance Fractional a => Monoid (Mean a) where
  mempty = Mean 0 0
  mappend !(Mean x n) !(Mean y k) = Mean ((x*n' + y*k') / (n' + k')) (n + k)
    where
      n' = fromIntegral n
      k' = fromIntegral k
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}