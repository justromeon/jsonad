{-# LANGUAGE DeriveGeneric, TupleSections, LambdaCase #-}
module JSONParser where

import Control.Applicative (Alternative(..))
import Data.Char (ord, isDigit, digitToInt)
import Data.List (intercalate)
import Data.Functor (($>))
import GHC.Generics (Generic)
import Numeric (showHex)

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
      Nothing -> Nothing
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