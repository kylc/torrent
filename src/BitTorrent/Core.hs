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

run :: String -> IO ()
run f = do
    infoM "BitTorrent.Core" $ "Downloading " ++ f

    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    trackerResponse <- newEmptyMVar

    let download = Download trackerResponse

    -- Request tracker information
    let metainfo = fromJust $ readMetainfo b
        req = makeRequest metainfo

    infoM "BitTorrent.Core" $ "Making tracker request to " ++ metainfo ^. announce

    response <- request req
    case response of
        Right r -> putMVar trackerResponse r
        Left e -> errorM "BitTorrent.Core" e

    infoM "BitTorrent.Core" "Received tracker response"

    -- Connect to peers

    -- Sleep forever
    forever $ delaySeconds 1
