
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Music.Prelude
import Music.Time.Internal.Convert
import Numeric.Natural
import Control.Applicative
import Text.Parsec
import Control.Applicative
import Numeric.Positive



data Rtm = Rtm BeatCount RtmList deriving (Eq, Ord, Show)

type BeatCount = Positive
type RtmList   = [RtmValue]

data RtmValue
    = RtmRest { _duration :: Positive } 
    | RtmNote { _isTiedFromPreviousNote :: Bool, _duration :: Positive }
    | RtmEmbed Rtm
    deriving (Eq, Ord, Show)

rhythmToScore :: Rtm -> Score StandardNote 
rhythmToScore (Rtm bc rl) = mfilter (\x -> all (== c) $ map (! 0) $ x^..pitches') $ stretchTo (realToFrac bc) $ scat $ map rhythmListToScore rl -- TODO: use BeatCount

rhythmListToScore :: RtmValue -> Score StandardNote
rhythmListToScore (RtmRest d)       = stretch (realToFrac d) g_
rhythmListToScore (RtmNote True d)  = stretch (realToFrac d) c 
rhythmListToScore (RtmNote False d) = endTie $ stretch (realToFrac d) c 
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



