module Network.BitTorrent.Metainfo
    ( announce
    , info
    , hash
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import Network.BitTorrent.Bencode

announce :: Bencode -> Maybe String
announce (BDict d) = do
    l <- Map.lookup "announce" d
    case l of
        (BString s) -> Just s
        _ -> Nothing
announce _ = fail "Expected dictionary"

info :: Bencode -> Maybe (Map.Map String Bencode)
info (BDict d) = do
    l <- Map.lookup "info" d
    case l of
        (BDict i) -> Just i
        _ -> Nothing
info _ = fail "Expected dictionary"

hash :: Bencode -> String
hash b = B.unpack $ SHA1.hash bs
  where bs = B.pack $ show b
