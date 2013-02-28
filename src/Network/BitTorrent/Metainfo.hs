module Network.BitTorrent.Metainfo where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import Network.BitTorrent.Bencode

announce :: Bencode -> Maybe String
announce (BDict d) = do
    (BString s) <- Map.lookup "announce" d
    return s

info :: Bencode -> Maybe (Map.Map String Bencode)
info (BDict d) = do
    (BDict i) <- Map.lookup "info" d
    return i

hash :: Bencode -> String
hash b = B.unpack $ SHA1.hash bs
  where bs = B.pack $ show b
