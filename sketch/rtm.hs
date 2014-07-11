{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Music.Prelude
import Music.Score.Convert
import Numeric.Natural
import Control.Applicative
import Text.Parsec
import Control.Applicative

newtype Positive = Positive { getPositive :: Natural } deriving
    ( 
    Eq,
    Enum,
    Integral,
    -- Data,
    -- Num,
    Ord,
    Read,
    Real,
    -- Ix,
    -- Typeable,
    -- Bits,
    -- Hashable,
    Whole
    )

instance Show Positive where
    show (Positive n) = show n

instance Num Positive where
  Positive n + Positive m = Positive (n + m)
  {-# INLINE (+) #-}
  Positive n * Positive m = Positive (n * m)
  {-# INLINE (*) #-}
  Positive n - Positive m | result < 0  = error "Positive.(-): negative result"
                          | result == 0 = error "Positive.(-): result zero"
                          | otherwise   = Positive result
    where result = n - m
  {-# INLINE (-) #-}
  abs (Positive n) = Positive n
  {-# INLINE abs #-}
  signum (Positive n) = Positive (signum n)
  {-# INLINE signum #-}
  fromInteger n
    | n > 0 = Positive (fromInteger n)
    | n == 0 = error "Positive.fromInteger: zero"
    | otherwise = error "Positive.fromInteger: negative"
  {-# INLINE fromInteger #-}




data Rtm = Rtm BeatCount RtmList deriving (Eq, Ord, Show)

type BeatCount = Positive
type RtmList   = [RtmValue]

data RtmValue
    = RtmRest { _duration :: Positive } 
    | RtmNote { _isTiedFromPreviousNote :: Bool, _duration :: Positive }
    | RtmEmbed Rtm
    deriving (Eq, Ord, Show)

rhythmToScore :: Rtm -> Score StandardNote 
rhythmToScore (Rtm bc rl) = mfilter (\x -> all (== c) $ map (! 0) $ x^..pitches') $ stretchTo (toDuration bc) $ scat $ map rhythmListToScore rl -- TODO: use BeatCount

rhythmListToScore :: RtmValue -> Score StandardNote
rhythmListToScore (RtmRest d)       = stretch (toDuration d) g_
rhythmListToScore (RtmNote True d)  = stretch (toDuration d) c 
rhythmListToScore (RtmNote False d) = endTie $ stretch (toDuration d) c 
rhythmListToScore (RtmEmbed r)      = rhythmToScore r

openRtm :: Rtm -> IO ()
openRtm = open . compress 4 . rhythmToScore


type Parser a = Parsec String () a

parseRtm :: String -> Either ParseError Rtm
parseRtm = doParse rhythmParser
  where
    doParse :: Parser a -> String -> Either ParseError a
    doParse p = parse p "No source"

rhythmParser :: Parser Rtm
rhythmParser = paren $ liftA2 Rtm (number <* space) rtmListParser
  where
    rtmListParser = paren $ sepBy1 rtmParser space
    rtmParser     = choice [restParser, noteParser, embedParser]
    noteParser    = choice [try $ RtmNote True <$> number <* dotParser <* zeros, RtmNote False <$> number]
    restParser    = RtmRest <$> (negSignParser *> number)
    embedParser   = RtmEmbed <$> (try (lookAhead paren1) *> rhythmParser)

    paren1        = char '('
    paren x       = char '(' *> x <* char ')'
    number        = fmap (fromInteger . read) $ (many1 digit)
    zeros         = many1 $ char '0'
    dotParser     = char '.'
    space         = char ' '
    negSignParser = char '-'
        














-- testRtm = Rtm 2 (replicate 3 $ RtmEmbed $ innerRtm)
--     where 
--         innerRtm = Rtm 1 (replicate 5 (RtmNote False 1))


-- (2 ((1 (1 1 1 1)) (1 (1 1 1 1)) (1 (1 1 1 1))))
-- (4 (1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1)))
-- (4 ((1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1))))

foo = openRtm $ fromRight $ parseRtm "(4 ((1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1))))"
fromRight (Right x) = x



