module Network.BitTorrent.Bencode
    ( Bencode(..)
    , parseBencodeFile
    ) where

import Control.Applicative hiding (many, optional)
import qualified Data.Map as Map

import Text.Parsec hiding ((<|>))
import Text.Parsec.ByteString (Parser, parseFromFile)

data Bencode = BInt Integer
             | BString String
             | BList [Bencode]
             | BDict (Map.Map String Bencode)
             deriving (Eq)

instance Show Bencode where
    show (BInt i) = "i" ++ show i ++ "e"
    show (BString s) = show (length s) ++ ":" ++ s
    show (BList xs) = "l" ++ concatMap show xs ++ "e"
    show (BDict m) = "d" ++ showElems m ++ "e"
      where
        showElems = Map.foldrWithKey showElem ""
        showElem k v z = k ++ show v ++ z

parseBencodeFile :: String -> IO (Either ParseError Bencode)
parseBencodeFile = parseFromFile bencode

bencode :: Parser Bencode
bencode = bInt
      <|> bString
      <|> bList
      <|> bDict

bInt :: Parser Bencode
bInt = BInt <$> (char 'i' *> number <* char 'e')

bString :: Parser Bencode
bString = do
    n <- number
    char ':'
    str <- count (fromInteger n) anyChar
    return $ BString str

bList :: Parser Bencode
bList = BList <$> (char 'l' *> many bencode <* char 'e')

bDict :: Parser Bencode
bDict = (BDict . Map.fromList) <$> (char 'd' *> many elem <* char 'e')
  where elem = do
          (BString key) <- bString
          val <- bencode
          return $ (key, val)

number :: Parser Integer
number = read <$> many1 digit
