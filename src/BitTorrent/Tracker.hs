module BitTorrent.Tracker
    ( request
    ) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (intToDigit, ord)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Word
import Network.HTTP

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.Types

request :: TrackerRequest -> IO (Either String TrackerResponse)
request req = do
    resp <- simpleHTTP $ getRequest url
    body <- getResponseBody resp
    code <- getResponseCode resp
    case code of
        (2, _, _) ->
            case parseBencode (B8.pack body) of
                Right bcode ->
                    case lookupString "peers" bcode of
                        Just cmp -> return . Right $ TrackerResponse { resInterval = 10
                                                                     , resPeers = decodePeers cmp
                                                                     }
                        Nothing -> return $ Left "Failed to find compact peer data"
                Left e -> fail $ "Failed to parse bencode: " ++ e
        (_, _, _) -> return . Left $ "Unexpected tracker response code: " ++ show code
  where
    url = reqAnnounce req ++ "?" ++ requestParams req


requestParams :: TrackerRequest -> String
requestParams req = urlEncodeVars
    [ ("info_hash", B8.unpack $ reqInfoHash req)
    , ("peer_id", reqPeerId req)
    , ("ip", reqIp req)
    , ("port", show $ reqPort req)
    , ("uploaded", show $ reqUploaded req)
    , ("downloaded", show $ reqDownloaded req)
    , ("left", show $ reqLeft req)
    , ("event", show $ reqEvent req)
    , ("compact", show $ reqCompact req)
    ]

decodePeers :: B.ByteString -> [Peer]
decodePeers bs | B.length bs < 6 = []
               | otherwise =
                   let (ip, r0) = B.splitAt 4 bs
                       (port, r1) = B.splitAt 2 r0
                       ip' = fromIntegral . roll $ B.unpack ip
                       port' = fromIntegral . roll $ B.unpack port
                   in  Peer "test123" ip' port' : decodePeers r1

roll :: [Word8] -> Int
roll = foldr unstep 0
  where unstep b a = a `shiftL` 8 .|. fromIntegral b
