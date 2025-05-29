module JsonParser where

import Control.Applicative

import Parse

data JValue = JNull
            | JBool Bool
            | JNumber Double
            | JString String
            | JArray [JValue]
            | JObject [(String,JValue)]
            deriving Show

jNull :: Parser JValue
jNull = JNull <$ string "null"

jBool :: Parser JValue
jBool = JBool <$> ((False <$ string "false") <|> (True <$ string "true"))