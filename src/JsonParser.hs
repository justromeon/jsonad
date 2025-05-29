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

jInt :: Parser Double
jInt = (0 <$ char '0') <|> (\n ns -> read (n:ns)) <$> nonZeroDig <*> digits

jFrac :: Parser Double
jFrac = read . ("0." ++) <$> (char '.' *> digits1) <|> pure 0

jExp :: Parser Double
jExp = oneOf "eE" *> (sign <*> (read <$> digits1)) <|> pure 0
  where
    sign = (negate <$ char '-') <|> (id <$ char '+') <|> pure id

jNumber :: Parser JValue
jNumber = do
    sign <- (negate <$ char '-') <|> pure id
    int  <- jInt
    frac <- jFrac
    expo <- jExp
    pure $ JNumber $ sign $ (int + frac) * 10 ** expo