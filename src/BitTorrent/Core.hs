module BitTorrent.Core
    ( run
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.PeerManager
import BitTorrent.Tracker
import BitTorrent.Types

run :: String -> IO ()
run f = do
    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    -- Request tracker information
    let metainfo = fromJust $ readMetainfo b
        req = TrackerRequest { reqAnnounce = mtAnnounce metainfo
                             , reqInfoHash = mtInfoHash metainfo
                             , reqPeerId = "abcdefghijklmnopqrst" -- TODO
                             , reqIp = "0.0.0.0" -- TODO
                             , reqPort = 5555 -- TODO
                             , reqUploaded = 0
                             , reqDownloaded = 0
                             , reqLeft = 0
                             , reqEvent = Started
                             , reqCompact = 1
                             }
    resp <- request req

    -- Connect to peers
    case resp of
        Right resp -> forkIO . runPeerMgr metainfo $ resPeers resp
        Left e -> fail $ "Failed to parse bencode file: " ++ e

    -- Download!
    forever $ delaySeconds 1

    return ()
