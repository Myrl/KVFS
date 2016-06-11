module Parser.KV where

import KV
import Text.Parsec
import Text.Parsec.Char

kvParser :: Parsec String () KV
kvParser = do
  spaces
  kvParser'

kvParser' :: Parsec String () KV
kvParser' = do
  key <- quotedText
  (Table key <$> tableParser) <|> (Entry key <$> quotedText)

quotedText :: Parsec String () String
quotedText = char '"' *> manyTill anyChar (char '"') <* spaces
  
tableParser :: Parsec String () [KV]
tableParser = do
  between (symbol "{") (symbol "}") (many kvParser)

symbol :: String -> Parsec String () String
symbol x = string x <* spaces
