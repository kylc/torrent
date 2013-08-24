module BitTorrent.Core
    ( run
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe

import System.Log.Logger

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.Tracker
import BitTorrent.Types

-- TODO: Send some real values here
makeTrackerRequest :: Metainfo -> TrackerRequest
makeTrackerRequest metainfo = TrackerRequest
    { reqAnnounce = mtAnnounce metainfo
    , reqInfoHash = mtInfoHash metainfo
    , reqPeerId = "abcdefghijklmnopqrst"
    , reqIp = "0.0.0.0"
    , reqPort = 5555
    , reqUploaded = 0
    , reqDownloaded = 0
    , reqLeft = 0
    , reqEvent = Started
    , reqCompact = 1
    }

run :: String -> IO ()
run f = do
    infoM "BitTorrent.Core" $ "Downloading " ++ f

    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    trackerResponse <- newEmptyMVar

    -- Request tracker information
    let metainfo = fromJust $ readMetainfo b
        req = makeTrackerRequest metainfo
    request req >>= putMVar trackerResponse

    -- Connect to peers

    -- Sleep forever
    forever $ delaySeconds 1
