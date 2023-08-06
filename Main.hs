module Main where

import Control.Applicative
import Data.Char
import Data.Tuple

type XMLAttribute = (String, String)
data XMLValue
  = XMLTag (String, [XMLAttribute], XMLValue)
  | XMLString String
  | XMLArray [XMLValue]
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Monad Parser where
  (Parser p1) >>= f =
    Parser $ \input -> do
      (input', output) <- p1 input
      let p2 = f output
      runParser p2 input'

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', a) <- p input
      Just (input', f a)

instance Applicative Parser where
  pure a = Parser (\x -> Just (x, a))
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f input
      | null input = Nothing
      | c == head input = Just (tail input, c)
      | otherwise = Nothing

parseString :: String -> Parser String
parseString = mapM parseChar

parseFailOnChar :: Char -> Parser ()
parseFailOnChar c = Parser f
  where
    f input
      | null input = Just (input, ())
      | c == head input = Nothing
      | otherwise = Just (input, ())

parseUntil :: (Char -> Bool) -> Parser String
parseUntil cond = Parser $ Just . swap . break cond

parseAttribute :: Parser XMLAttribute
parseAttribute =
  (,)
    <$> (parseUntil (\x -> x == '=' || x == '>') <* parseChar '=')
    <*> (parseChar '"' *> parseUntil (== '"') <* parseChar '"')

parseWS :: Parser String
parseWS = parseUntil (not . isSpace)

failOnEmpty :: Parser [a] -> Parser [a]
failOnEmpty p =
  Parser $ \input -> do
    (input', output) <- runParser p input
    if null output
      then Nothing
      else Just (input', output)

parseAttributes :: Parser [XMLAttribute]
parseAttributes = many (parseWS *> parseAttribute)

parseTags :: Parser XMLValue
parseTags = XMLArray <$> failOnEmpty (many (parseWS *> parseTag))

parseTag :: Parser XMLValue
parseTag =
  (\name attrs body endName -> XMLTag (ensure name endName, attrs, body))
    <$> (parseChar '<'
           *> parseFailOnChar '/'
           *> parseUntil (\c -> (c == '>') || isSpace c))
    <*> ((parseWS *> ([] <$ parseChar '>'))
           <|> (parseAttributes <* parseChar '>'))
    <*> (parseTags <|> (XMLString <$> parseUntil (== '<')))
    <*> (parseWS *> parseString "</" *> parseUntil (== '>') <* parseChar '>')
  where
    ensure a b
      | a == b = a
      | otherwise =
        error
          $ "Tag begins and ends with a different name '"
              ++ a
              ++ "' - '"
              ++ b
              ++ "'"

parseRoot :: Parser XMLValue
parseRoot =
  (\attrs body -> XMLTag ("root", attrs, body))
    <$> (parseString "<?xml" *> parseAttributes <* parseString "?>")
    <*> parseTags

main :: IO ()
main = undefined
