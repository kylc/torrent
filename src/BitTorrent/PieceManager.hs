module BitTorrent.PieceManager
    ( runPieceMgr
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Distributed.Process
import Control.Monad

import Data.Array

import BitTorrent.Types

runPieceMgr :: Metainfo -> Process ()
runPieceMgr m = do
    let pieceCount = length $ mtPieces m
    mdb <- liftIO $ newMVar $ initDb pieceCount

    dbUpdaterPid <- spawnLocal $ runDbUpdater mdb
    register "db_updater" dbUpdaterPid

    -- Wait a bit for all Have messages to come in
    liftIO $ delaySeconds 3

    fetchPieces pieceCount mdb

fetchPieces :: Int -> MVar (Array Int [ProcessId]) -> Process ()
fetchPieces pieceCount mdb = do
    -- Iterate linearly over all pieces
    forM_ [0..(pieceCount - 1)] $ \i -> do
        say $ "Downloading piece " ++ show i ++ "/" ++ show pieceCount
        liftIO $ delaySeconds 1

        -- Grab the list of all peers who have this piece
        db <- liftIO $ readMVar mdb
        let peers = db ! i

        unless (null peers) $ do
          -- Tell the first peer in the list to download this piece
          say $ "Requesting peer " ++ show (head peers) ++ " download piece " ++ show i
          send (head peers) $ PeerFetch i

runDbUpdater :: MVar (Array Int [ProcessId]) -> Process ()
runDbUpdater mdb = do
    (pid, (PeerHas n)) <- expect :: Process (ProcessId, ProcMessage)
    liftIO $ modifyMVar_ mdb $ \v -> return $ updateDb v pid n
    runDbUpdater mdb

initDb :: Int -> Array Int [ProcessId]
initDb pc = array (0, pc - 1) [(x, []) | x <- [0..(pc - 1)]]

updateDb :: Array Int [ProcessId] -> ProcessId -> Int -> Array Int [ProcessId]
updateDb db pid n = db // [(n, pid : db ! n)]
