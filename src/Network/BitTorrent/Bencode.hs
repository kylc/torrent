module Network.BitTorrent.Bencode
    ( Bencode(..)
    , parseBencodeFile
    ) where

import qualified Data.Map as Map

import Text.Parsec
import Text.Parsec.ByteString (Parser, parseFromFile)

data Bencode = BInt Integer
             | BString String
             | BList [Bencode]
             | BDict [(String, Bencode)]
             deriving (Show, Eq)

parseBencodeFile :: String -> IO (Either ParseError Bencode)
parseBencodeFile = parseFromFile bencode

bencode :: Parser Bencode
bencode = bInt
      <|> bString
      <|> bList
      <|> bDict

bInt :: Parser Bencode
bInt = do
    n <- between (string "i") (string "e") (number)
    return $ BInt n

bString :: Parser Bencode
bString = do
    n <- number
    char ':'
    str <- count (fromInteger n) anyChar
    return $ BString str

bList :: Parser Bencode
bList = do
    char 'l'
    xs <- many bencode
    char 'e'
    return $ BList xs

bDict :: Parser Bencode
bDict = do
    char 'd'
    xs <- many elem
    char 'e'
    return $ BDict xs
  where
    elem :: Parser (String, Bencode)
    elem = do
      (BString key) <- bString
      val <- bencode
      return $ (key, val)

number :: Parser Integer
number = do
  n <- many1 digit
  return $ read n
