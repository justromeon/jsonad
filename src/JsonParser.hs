{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module JsonParser where

import Control.Applicative

data JValue = JNull
            | JBool Bool
            | JNumber {int :: Integer, frac :: [Int], exponent :: Integer}
            | JString String
            | JArray [JValue]
            | JObject [(String,JValue)]
            deriving Show

newtype Parser a = Parser { runP :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \inp -> case runP p inp of
        Nothing     -> Nothing
        Just (xs,x) -> Just (xs,f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \inp -> Just (inp,x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = Parser $ \inp -> case runP pf inp of
        Nothing     -> Nothing
        Just (xs,f) -> runP (fmap f px) xs

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = Parser $ \inp -> case runP px inp of
        Nothing     -> Nothing
        Just (xs,x) -> runP (f x) xs

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = Parser $ \inp -> case runP px inp of
        Nothing     -> runP py inp
        Just (xs,x) -> Just (xs,x)

--Parsing primitives
char :: Char -> Parser Char
char c = Parser $ \case
    x:xs | x == c -> Just (xs,x)
    _             -> Nothing

string :: String -> Parser String
string = traverse char