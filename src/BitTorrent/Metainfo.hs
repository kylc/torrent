module BitTorrent.Metainfo
    ( Hash
    , Metainfo(..)
    , MetainfoFile(..)
    , readMetainfo
    ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

import BitTorrent.Bencode
import BitTorrent.Types

readMetainfo :: Bencode -> Maybe Metainfo
readMetainfo b = do
    ann <- fmap B8.unpack $ lookupString "announce" b
    info <- lookupDict "info" b
    let infoHash = hash info
    name <- lookupString "name" info
    pieceLen <- lookupInt "piece length" info
    pieces <- fmap readPieces $ lookupDict "pieces" info
    case lookupDict "length" info of
        Just (BInt len) -> 
            return $ Metainfo ann infoHash name pieceLen pieces (Just len) Nothing
        Nothing -> do
            let files = readFiles =<< lookupDict "files" info
            return $ Metainfo ann infoHash name pieceLen pieces Nothing files

readPieces :: Bencode -> [Hash]
readPieces (BString s) = undefined
--    | B.null s = []
--    | otherwise = B.append $ (B.take 20 s) (readPieces (BString $ B.drop 20 s))

readFiles :: Bencode -> Maybe [MetainfoFile]
readFiles = undefined
