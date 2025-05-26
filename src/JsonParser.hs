module JsonParser where

data JValue = JNull
            | JBool Bool
            | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
            | JString String
            | JArray [JValue]
            | JObject [(String,JValue)]
            deriving Show
