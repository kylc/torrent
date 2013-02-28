import System.Environment

import BitTorrent.Core

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] -> run x
        _ -> putStrLn "Usage: torrent <file.torrent>"
