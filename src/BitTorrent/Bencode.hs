module BitTorrent.Bencode
    ( Bencode(..)
    , parseBencode
    , parseBencodeFile
    , lookupDict
    , lookupInt
    , lookupString
    , hash
    ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as A8
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

data Bencode = BInt Integer
             | BString B.ByteString
             | BList [Bencode]
             | BDict (Map.Map String Bencode)
             deriving (Eq)

instance Show Bencode where
    show (BInt i) = "i" ++ show i ++ "e"
    show (BString s) = show (B.length s) ++ ":" ++ B8.unpack s
    show (BList xs) = "l" ++ concatMap show xs ++ "e"
    show (BDict m) = "d" ++ showElems m ++ "e"
      where
        showElems = Map.foldrWithKey showElem ""
        showElem k v z = show (BString $ B8.pack k) ++ show v ++ z

parseBencode :: B.ByteString -> Either String Bencode
parseBencode = parseOnly bencode

parseBencodeFile :: String -> IO Bencode
parseBencodeFile f = Streams.withFileAsInput f (Streams.parseFromStream bencode)

bencode :: Parser Bencode
bencode = bInt
      <|> bString
      <|> bList
      <|> bDict

bInt :: Parser Bencode
bInt = BInt <$> (char 'i' *> signed decimal <* char 'e')

bString :: Parser Bencode
bString = do
    n <- decimal
    char ':'
    str <- A8.take n
    return $ BString str

bList :: Parser Bencode
bList = BList <$> (char 'l' *> many' bencode <* char 'e')

bDict :: Parser Bencode
bDict = (BDict . Map.fromList) <$> (char 'd' *> many' elem <* char 'e')
  where elem = do
          (BString key) <- bString
          val <- bencode
          return (B8.unpack key, val)

lookupDict :: String -> Bencode -> Maybe Bencode
lookupDict s (BDict d) = Map.lookup s d
lookupDict s _ = Nothing

lookupInt :: String -> Bencode -> Maybe Integer
lookupInt k d = case lookupDict k d of
                       Just (BInt i) -> Just i
                       _ -> Nothing

lookupString :: String -> Bencode -> Maybe B8.ByteString
lookupString k d = case lookupDict k d of
                       Just (BString s) -> Just s
                       _ -> Nothing

hash :: Bencode -> B.ByteString
hash b = SHA1.hash bs
  where bs = B8.pack $ show b
