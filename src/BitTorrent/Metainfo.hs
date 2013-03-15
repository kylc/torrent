module BitTorrent.Metainfo
    ( Hash
    , Metainfo(..)
    , MetainfoFile(..)
    , readMetainfo
    ) where

import Control.Applicative
import qualified Data.Map as Map

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import BitTorrent.Bencode
import BitTorrent.Types

readMetainfo :: Bencode -> Maybe Metainfo
readMetainfo b = do
    ann <- fmap B8.unpack $ lookupString "announce" b
    info <- lookupDict "info" b
    let infoHash = hash info
    name <- lookupString "name" info
    pieceLen <- lookupInt "piece length" info
    pieces <- fmap readPieces $ lookupString "pieces" info
    let len = lookupInt "length" info
    let files = readFiles =<< lookupDict "files" info
    return $ Metainfo ann infoHash name pieceLen pieces len files

readPieces :: B.ByteString -> [Hash]
readPieces b
    | B.null b = []
    | otherwise = let (h, r) = B.splitAt 20 b
                  in h : readPieces r

readFiles :: Bencode -> Maybe [MetainfoFile]
readFiles = undefined
