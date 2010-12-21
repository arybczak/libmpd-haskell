-- | Module    : Network.MPD.Utils
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Utilities.

module Network.MPD.Utils (
    parseIso8601, parseHost, parseNum, parseInt,
    parseInteger, parseFrac, parseBool, showBool,
    breakChar, parseTriple, toAssoc
    ) where

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as B
import Data.Time.Format (ParseTime, parseTime)
import System.Locale (defaultTimeLocale)

-- Break a string by character, removing the separator.
breakChar :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
breakChar c s = second (B.drop 1) (B.break (== c) s)

-- Parse date in iso 8601 format
parseIso8601 :: (ParseTime t) => B.ByteString -> Maybe t
parseIso8601 = parseTime defaultTimeLocale "%FT%TZ" . B.unpack

-- | Splits host into (password, host) pair
parseHost :: String -> (String, String)
parseHost h =
    if '@' `elem` h
       then second (drop 1) $ break (=='@') h
       else ("", h)

-- Parse numeric value, returning 'Nothing' on failure.
parseNum :: (Read a) => String -> Maybe a
parseNum s = do
    [(x, "")] <- return $ reads s
    return x

-- Parse a positive or negative integer value, returning 'Nothing' on failure.
parseNum' :: (a -> Maybe (b, B.ByteString)) -> a -> Maybe b
parseNum' f s = do
    (x, rest) <- f s
    if (B.null rest)
       then Just x
       else Nothing

parseInt :: B.ByteString -> Maybe Int
parseInt = parseNum' B.readInt

parseInteger :: B.ByteString -> Maybe Integer
parseInteger = parseNum' B.readInteger

-- Parse C style floating point value, returning 'Nothing' on failure.
parseFrac :: (Fractional a, Read a) => B.ByteString -> Maybe a
parseFrac s =
    case s' of
        "nan"  -> return $ read "NaN"
        "inf"  -> return $ read "Infinity"
        "-inf" -> return $ read "-Infinity"
        _      -> do [(x, "")] <- return $ reads s'
                     return x
    where
          s' = B.unpack s

-- Inverts 'parseBool'.
showBool :: Bool -> String
showBool x = if x then "1" else "0"

-- Parse a boolean response value.
parseBool :: B.ByteString -> Maybe Bool
parseBool s = if B.null s
                 then Nothing
                 else case B.head s of
                  '1' -> Just True
                  '0' -> Just False
                  _   -> Nothing

-- Break a string into triple.
parseTriple :: Char -> (B.ByteString -> Maybe a) -> B.ByteString -> Maybe (a, a, a)
parseTriple c f s =
    let (u, u') = breakChar c s
        (v, w)  = breakChar c u' in
    case (f u, f v, f w) of
        (Just x, Just y, Just z) -> Just (x, y, z)
        _                        -> Nothing

-- Break a string into an key-value pair, separating at the first ':'.
toAssoc :: B.ByteString -> (String, B.ByteString)
toAssoc x = (B.unpack k, B.dropWhile (== ' ') $ B.drop 1 v)
    where
          (k,v) = B.break (== ':') x
