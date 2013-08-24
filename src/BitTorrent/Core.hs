module BitTorrent.Core
    ( run
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Concurrent.MVar
import Control.Monad
import Control.Lens
import Data.Maybe

import System.Log.Logger

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.Tracker
import BitTorrent.Types

-- TODO: Send some real values here
makeTrackerRequest :: Metainfo -> TrackerRequest
makeTrackerRequest metainfo = TrackerRequest
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

run :: String -> IO ()
run f = do
    infoM "BitTorrent.Core" $ "Downloading " ++ f

    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    trackerResponse <- newEmptyMVar

    -- Request tracker information
    let metainfo = fromJust $ readMetainfo b
        req = makeTrackerRequest metainfo

    infoM "BitTorrent.Core" $ "Making tracker request to " ++ metainfo ^. announce
    request req >>= putMVar trackerResponse
    infoM "BitTorrent.Core" $ "Received tracker response"

    -- Connect to peers

    -- Sleep forever
    forever $ delaySeconds 1
