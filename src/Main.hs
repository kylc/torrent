import System.Environment
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)

import BitTorrent.Core

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName $ setLevel INFO

    args <- getArgs
    case args of
        [x] -> run x
        _ -> putStrLn "Usage: torrent <file.torrent>"
