module BitTorrent.Tracker
    ( request
    , makeRequest
    ) where

import Control.Lens
import Data.Bits
import Data.Maybe (fromJust)
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Network.HTTP hiding (port)

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.Types

request :: TrackerRequest -> IO (Either String TrackerResponse)
request req = do
    resp <- simpleHTTP $ getRequest url
    body <- getResponseBody resp
    code <- getResponseCode resp
    case code of
        (2, _, _) -> return $ readBody body
        (_, _, _) -> return . Left $ "Unexpected tracker response code: " ++ show code
  where
    url = req ^. announce ++ "?" ++ requestParams req

makeRequest :: Metainfo -> TrackerRequest
makeRequest metainfo = TrackerRequest
    { _reqAnnounce = metainfo ^. announce
    , _reqInfoHash = metainfo ^. infoHash
    , _reqPeerId = "abcdefghijklmnopqrst"
    , _reqIp = "0.0.0.0"
    , _reqPort = 5555
    , _reqUploaded = 0
    , _reqDownloaded = 0
    , _reqLeft = 0
    , _reqEvent = Started
    , _reqCompact = 1
    }

readBody :: String -> Either String TrackerResponse
readBody body = do
    bcode <- parseBencode $ B8.pack body
    interval <- case lookupInt "interval" bcode of
                  Just i -> Right $ fromIntegral i
                  Nothing -> Left "No interval key in tracker response"
    peers <- case lookupString "peers" bcode of
                  Just p -> Right $ decodePeers p
                  Nothing -> Left "No peers key in tracker response"
    return $ TrackerResponse interval peers


requestParams :: TrackerRequest -> String
requestParams req = urlEncodeVars
    [ ("info_hash", B8.unpack $ req ^. infoHash)
    , ("peer_id", req ^. peerId)
    , ("ip", req ^. ip)
    , ("port", show $ req ^. port)
    , ("uploaded", show $ req ^. uploaded)
    , ("downloaded", show $ req ^. downloaded)
    , ("left", show $ req ^. left)
    , ("event", show $ req ^. event)
    , ("compact", show $ req ^. compact)
    ]

decodePeers :: B.ByteString -> [Peer]
decodePeers bs | B.length bs < 6 = []
               | otherwise =
                   let (ip, r0) = B.splitAt 4 bs
                       (port, r1) = B.splitAt 2 r0
                       ip' = fromIntegral . roll $ B.unpack ip
                       port' = fromIntegral . roll $ B.unpack port
                   in  Peer Nothing ip' port' : decodePeers r1

roll :: [Word8] -> Int
roll = foldr unstep 0
  where unstep b a = a `shiftL` 8 .|. fromIntegral b
