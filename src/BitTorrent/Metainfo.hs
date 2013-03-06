module BitTorrent.Metainfo
    ( Hash
    , Metainfo(..)
    , MetainfoFile(..)
    , readMetainfo
    ) where

import Control.Applicative
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import BitTorrent.Bencode
import BitTorrent.Types


readMetainfo :: Bencode -> Maybe Metainfo
readMetainfo (BDict d) = do
    (BString ann) <- Map.lookup "announce" d
    (BDict info) <- Map.lookup "info" d
    let infoHash = hash $ BDict info
    (BString name) <- Map.lookup "name" info
    (BInt pieceLen) <- Map.lookup "piece length" info
    pieces <- (return . readPieces) =<< Map.lookup "pieces" info
    let lenM = Map.lookup "length" info
    case lenM of
        Just (BInt len) -> 
            return $ Metainfo ann infoHash name pieceLen pieces (Just len) Nothing
        Nothing -> do
            let files = readFiles =<< Map.lookup "files" info
            return $ Metainfo ann infoHash name pieceLen pieces Nothing files

readPieces :: Bencode -> [Hash]
readPieces (BString "") = []
readPieces (BString s) = (take 20 s) : readPieces (BString $ drop 20 s)

readFiles :: Bencode -> Maybe [MetainfoFile]
readFiles = undefined

hash :: Bencode -> String
hash b = B.unpack $ SHA1.hash bs
  where bs = B.pack $ show b
