{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module JSONParser where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (ord, isDigit, digitToInt, chr, isHexDigit, isSpace)
import Data.List (intercalate)
import Data.Functor (($>))
import GHC.Generics (Generic)
import Numeric (showHex)
import Test.QuickCheck hiding (Positive, Negative)

data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: [Int], exponent :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Generic)

instance Show JValue where
    show = \case
      JNull          -> "null"
      JBool True     -> "true"
      JBool False    -> "false"
      JString s      -> showJSONString s
      JNumber i [] 0 -> show i
      JNumber i f  0 -> show i ++ "." ++ concatMap show f
      JNumber i [] e -> show i ++ "e" ++ show e
      JNumber i f  e -> show i ++ "." ++ concatMap show f ++ "e" ++ show e
      JArray a       -> "[" ++ intercalate ", " (map show a)     ++ "]"
      JObject o      -> "{" ++ intercalate ", " (map showPair o) ++ "}"
      where
        showPair (k,v) = showJSONString k ++ ": " ++ show v

showJSONString :: String -> String
showJSONString s = "\"" ++ concatMap showJSONChar s ++ "\""

isControl :: Char -> Bool
isControl c = c `elem` ['\0' .. '\31']

showJSONChar :: Char -> String
showJSONChar = \case
  '\'' -> "'"
  '\"' -> "\\\""
  '\\' -> "\\\\"
  '/'  -> "\\/"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c | isControl c -> "\\u" ++ drop (length paddedHex - 4) paddedHex
    where paddedHex = "0000" ++ showHex (ord c) ""
  c -> [c]

--Parser declaration and typeclass instances
newtype Parser i o = Parser { runParser :: i -> Maybe (i,o) }

instance Functor (Parser i) where
  fmap f parser = Parser $ \inp -> case runParser parser inp of
      Nothing      -> Nothing
      Just (xs, x) -> Just (xs, f x)

instance Applicative (Parser i) where
  pure x = Parser $ pure . (, x)
  pf <*> po = Parser $ \inp -> case runParser pf inp of
    Nothing      -> Nothing
    Just (xs, f) -> runParser (fmap f po) xs

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  px <|> py = Parser $ \inp -> case runParser px inp of
    Nothing -> runParser py inp
    success -> success

instance Monad (Parser i) where
  p >>= f = Parser $ \inp -> case runParser p inp of
    Nothing     -> Nothing
    Just (xs,x) -> runParser (f x) xs

--Parsers
satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = Parser $ \case
    (x:xs) | p x -> Just (xs,x)
    _            -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

string :: String -> Parser String String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

jNull :: Parser String JValue
jNull = string "null" $> JNull

jBool :: Parser String JValue
jBool = string "true"  $> JBool True
    <|> string "false" $> JBool False

jsonChar :: Parser String Char
jsonChar =   string "\\\"" $> '"'
         <|> string "\\\\" $> '\\'
         <|> string "\\/"  $> '/'
         <|> string "\\b"  $> '\b'
         <|> string "\\f"  $> '\f'
         <|> string "\\n"  $> '\n'
         <|> string "\\r"  $> '\r'
         <|> string "\\t"  $> '\t'
         <|> unicodeChar
         <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
  where
    unicodeChar = chr . fromIntegral . digitsToNumber 16 0
              <$> (string "\\u" *> replicateM 4 hexDigit)

    hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> Integer -> [Int] -> Integer
digitsToNumber base = foldl (\num d -> num * fromIntegral base + fromIntegral d)

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
  where
    jString' = do
      opFirst <- optional jsonChar
      case opFirst of
        Nothing -> char '"' $> ""
        Just first | not (isSurrogate first) -> (first:) <$> jString'
        Just first -> do
          second <- jsonChar
          if isHighSurrogate first && isLowSurrogate second
          then (combineSurrogates first second :) <$> jString'
          else empty

highSurrogateLowerBound, highSurrogateUpperBound :: Int
highSurrogateLowerBound = 0xD800
highSurrogateUpperBound = 0xDBFF

lowSurrogateLowerBound, lowSurrogateUpperBound :: Int
lowSurrogateLowerBound  = 0xDC00
lowSurrogateUpperBound  = 0xDFFF

isHighSurrogate, isLowSurrogate, isSurrogate :: Char -> Bool
isHighSurrogate a =
  ord a >= highSurrogateLowerBound && ord a <= highSurrogateUpperBound
isLowSurrogate a  =
  ord a >= lowSurrogateLowerBound && ord a <= lowSurrogateUpperBound
isSurrogate a     = isHighSurrogate a || isLowSurrogate a

combineSurrogates :: Char -> Char -> Char
combineSurrogates a b = chr $
  ((ord a - highSurrogateLowerBound) `shiftL` 10)
  + (ord b - lowSurrogateLowerBound) + 0x10000

digits :: Parser String [Int]
digits = some digit

digit1To9 :: Parser String Int
digit1To9 = digitToInt <$> satisfy (\c -> isDigit c && c /= '0')

jUInt :: Parser String Integer
jUInt = (\d ds -> digitsToNumber 10 0 (d:ds) ) <$> digit1To9 <*> digits
    <|> fromIntegral <$> digit

signInt :: Maybe Char -> Integer -> Integer
signInt (Just '-') i = negate i
signInt _          i = i

jInt' :: Parser String Integer
jInt' = signInt <$> optional (char '-') <*> jUInt

jFrac :: Parser String [Int]
jFrac = char '.' *> digits

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E')
    *> (signInt <$> optional (char '+' <|> char '-') <*> jInt')

jInt :: Parser String JValue
jInt = JNumber <$> jInt' <*> pure [] <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jInt' <*> pure [] <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = JNumber <$> jInt' <*> jFrac <*> pure 0

jIntFracExp :: Parser String JValue
jIntFracExp = do
  jv <- jIntFrac
  e  <- jExp
  case jv of
    JNumber i f _ -> pure (JNumber i f e)
    _             -> pure jv

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntFrac <|> jIntExp <|> jInt

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy content wrapper = wrapper *> content <* wrapper

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy value sep = (:) <$> value <*> many (sep *> value)

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray = JArray <$>
  (char '[' *> spaces
  *> many (jValue <* optional (char ','))
  <* spaces <* char ']')

jObject :: Parser String JValue
jObject = JObject <$>
  (char '{' *> spaces
  *> many (pair <* optional (char ','))
  <* spaces <* char '}')
  where
    pair = do
      s <- jString `surroundedBy` spaces
      _ <- char ':' *> spaces
      jv <- jValue
      case s of
        JString str -> pure (str, jv)
        _           -> pure ("", JNull)

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' = jNull
          <|> jBool
          <|> jString
          <|> jNumber
          <|> jArray
          <|> jObject

parseJSON :: String -> Maybe JValue
parseJSON s = case runParser jValue s of
  Just ("", json) -> Just json
  _               -> Nothing

--QuickCheck Generators for testing

--Scalar Generators
jNullGen :: Gen JValue
jNullGen = pure JNull

jBoolGen :: Gen JValue
jBoolGen = JBool <$> arbitrary

jNumberGen :: Gen JValue
jNumberGen = JNumber <$> arbitrary <*> listOf (choose (0,9)) <*> arbitrary

jsonStringGen :: Gen String
jsonStringGen =
  concat <$> listOf (oneof [ vectorOf 4 arbitraryUnicodeChar
                           , escapedUnicodeChar ])
  where
    escapedUnicodeChar = ("\\u" ++) <$> vectorOf 4 (elements hexDigitLetters)
    hexDigitLetters = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

jStringGen :: Gen JValue
jStringGen = JString <$> jsonStringGen

--Composite Generators
jArrayGen :: Int -> Gen JValue
jArrayGen = fmap JArray . scale (`div` 2) . listOf . jValueGen . (`div` 2)

jObjectGen :: Int -> Gen JValue
jObjectGen = fmap JObject . scale (`div` 2) . listOf . objKV . (`div` 2)
  where
    objKV n = (,) <$> jsonStringGen <*> jValueGen n

jValueGen :: Int -> Gen JValue
jValueGen n
    | n < 5     = frequency [(4, oneof scalarGens), (1, oneof $ compositeGens n)]
    | otherwise = frequency [(1, oneof scalarGens), (4, oneof $ compositeGens n)]
  where
    scalarGens      = [jNullGen, jBoolGen, jNumberGen, jStringGen]
    compositeGens m = [jArrayGen m, jObjectGen m]

jsonWhitespaceGen :: Gen String
jsonWhitespaceGen =
  scale (round . sqrt . (fromIntegral :: Int -> Double))
  . listOf
  . elements
  $ [' ' , '\n' , '\r' , '\t']

stringify :: JValue -> Gen String
stringify = pad . go
  where
    surround l r j = l ++ j ++ r
    pad gen = surround <$> jsonWhitespaceGen <*> jsonWhitespaceGen <*> gen
    commaSeparated = pad . pure . intercalate ","

    go value = case value of
      JArray es ->
        mapM (pad . stringify) es
          >>= fmap (surround "[" "]") . commaSeparated
      JObject kvs ->
        mapM stringifyKV kvs >>= fmap (surround "{" "}") . commaSeparated
      _           -> return $ show value

    stringifyKV (k, v) =
      surround <$> pad (pure $ showJSONString k) <*> stringify v <*> pure ":"

instance Arbitrary JValue where
  arbitrary :: Gen JValue
  arbitrary = sized jValueGen

  shrink :: JValue -> [JValue]
  shrink = genericShrink

--QuickCheck properties
prop_genParseJString :: Property
prop_genParseJString =
  forAllShrink jStringGen shrink $ \js ->
    case runParser jString (show js) of
      Nothing    -> False
      Just (_,o) -> o == js

prop_genParseJNumber :: Property
prop_genParseJNumber =
  forAllShrink jNumberGen shrink $ \jn ->
    case runParser jNumber (show jn) of
      Nothing    -> False
      Just (_,o) -> o == jn

prop_genParseJArray :: Property
prop_genParseJArray =
  forAllShrink (sized jArrayGen) shrink $ \ja -> do
    jas <- dropWhile isSpace <$> stringify ja
    return . counterexample (show jas) $ case runParser jArray jas of
      Nothing    -> False
      Just (_,o) -> o == ja

prop_genParseJObject :: Property
prop_genParseJObject =
  forAllShrink (sized jObjectGen) shrink $ \jo -> do
    jos <- dropWhile isSpace <$> stringify jo
    return . counterexample (show jos) $ case runParser jObject jos of
      Nothing    -> False
      Just (_,o) -> o == jo

prop_genParseJSON :: Property
prop_genParseJSON = forAllShrink (sized jValueGen) shrink $ \value -> do
  json <- stringify value
  return . counterexample (show json) . (== Just value) . parseJSON $ json