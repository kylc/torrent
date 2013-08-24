{-# LANGUAGE DeriveDataTypeable #-} 

module BitTorrent.Options
    ( Options
    , optPort
    ) where

import System.Console.CmdArgs

data Options = Options
    { optPort :: Integer
    } deriving (Show, Data, Typeable)

options = Options
    { optPort = def &= explicit &= name "port" &= help "Listen port"
    }
    &= summary "Torrent client"
