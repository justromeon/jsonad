{-# LANGUAGE DeriveGeneric, LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
module JSONParser where

import Data.Char (ord)
import Data.List (intercalate)
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
    show :: JValue -> String
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