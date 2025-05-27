module JsonParser where

import Control.Applicative

import Parse

data JValue = JNull
            | JBool Bool
            | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
            | JString String
            | JArray [JValue]
            | JObject [(String,JValue)]
            deriving Show

jNull :: Parser JValue
jNull = JNull <$ string "null"

jBool :: Parser JValue
jBool = JBool <$> ((False <$ string "false") <|> (True <$ string "true"))