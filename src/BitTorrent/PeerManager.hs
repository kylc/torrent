module BitTorrent.PeerManager
    ( runPeerMgr
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Monad.Reader
import Control.Monad.State
import Data.Array.IArray
import Data.Array.Unboxed

import BitTorrent.Peer
import BitTorrent.Types

runPeerMgr :: Metainfo -> [Peer] -> IO ()
runPeerMgr m ps = do
    -- Open connections
    forM_ ps $ \p -> forkIO $
        let st = defaultPeerState $ length (mtPieces m)
        in void $ execStateT (runReaderT (runPeer p) m) st

    -- Wait a bit for all incoming bitfields and haves to arrive
    -- TODO: How long should we wait?  Or should we have a smarter approach?
    delaySeconds 3

    -- Download each piece
    -- TODO: This is really dumb right now.  Should randomly choose pieces based
    -- on availability and try to choose a fast peer.

    let pieceCount = length $ mtPieces m
        status = listArray (0, pieceCount) $ replicate pieceCount PieceEmpty :: Array Int PieceState

    forM_ [1..pieceCount] $ \i -> do
        let piece = mtPieces m !! i
            s = status ! i
            -- peer = head $ filter (\p -> peerHas p ! i) ps
        return ()

emptyPieces :: [PieceState] -> [PieceState]
emptyPieces = filter $ \x ->
    case x of
        PieceEmpty -> False
        _ -> True
