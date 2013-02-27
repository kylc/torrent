module Network.BitTorrent.Metainfo where

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
