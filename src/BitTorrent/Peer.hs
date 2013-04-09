module BitTorrent.Peer
    ( runPeer
    ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char (ord)
import Data.Word

import Control.Distributed.Process
import Control.Monad.State
import Data.Array.IArray
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Binary as A
import qualified Data.ByteString as B
import Network.Socket (Socket)
import qualified Network.Socket as N
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import BitTorrent.Types

runPeer :: Metainfo -> Peer -> Process ()
runPeer meta peer = do
    pid <- getSelfPid
    conn <- spawnLocal $ runPeerConnection meta peer pid
    go $ defaultPeerState $ pieceCount meta
  where
    pieceCount = length . mtPieces
    go :: PeerState -> Process ()
    go st = do
      m <- expect :: Process ProcMessage
      newst <- case m of
                   PeerRecv msg -> handleMessage (pieceCount meta) msg st
                   PeerFetch i -> return st
      go newst

runPeerConnection :: Metainfo -> Peer -> ProcessId -> Process ()
runPeerConnection meta peer pid = do
    sock <- liftIO $ peerConnect (peerIp peer) (peerPort peer)
    (is, os) <- liftIO $ Streams.socketToStreams sock

    say $ "Connected to: " ++ show peer

    handshake <- do
        message <- liftIO $ do
            peerHandshake os (mtInfoHash meta)
            peerListen is parseHandshake
        send pid $ PeerRecv message

    forever $ do
        message <- liftIO $ peerListen is parseMessage
        send pid $ PeerRecv message

peerConnect :: Word32 -> Word16 -> IO Socket
peerConnect a p = do
    sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
    N.connect sock $ addr a p
    return sock
  where
    addr a p = N.SockAddrInet (N.PortNum . fromIntegral $ p) a

peerHandshake :: Streams.OutputStream B.ByteString -> Hash -> IO ()
peerHandshake is ih = void $
    flip Streams.write is $ Just $
        B.concat [ B.singleton protoHeaderSize
                 , strToBS protoHeader
                 , B.pack protoReserved
                 , ih
                 , strToBS protoPeerId ]
  where
    protoHeaderSize = fromIntegral . length $ protoHeader
    protoHeader = "BitTorrent protocol"
    protoReserved = replicate 8 0
    protoPeerId = "-HT0001-asdefghjasdh"
    strToBS = B.pack . map (fromIntegral . ord)

peerListen :: Streams.InputStream B.ByteString -> A.Parser PeerMessage -> IO PeerMessage
peerListen is parser = Streams.parseFromStream parser is

parseMessage :: A.Parser PeerMessage
parseMessage = do
    len <- A.anyWord8
    if len == 0
        then return KeepAlive
        else parseMessage' $ fromIntegral len
  where
    parseMessage' len = do
        id <- A.anyWord8
        case id of
            0 -> return Choke
            1 -> return Unchoke
            2 -> return Interested
            3 -> return NotInterested
            4 -> Have <$> w32
            5 -> Bitfield <$> A.take (len - 1)
            6 -> Request <$> w32 <*> w32 <*> w32
            7 -> Piece <$> w32 <*> w32 <*> A.take (len - 9)
            8 -> Cancel <$> w32 <*> w32 <*> w32
            9 -> Port <$> w32
            _ -> fail $ "Invalid message id: " ++ show id
    w32 = fromIntegral <$> A.anyWord32be

parseHandshake :: A.Parser PeerMessage
parseHandshake =
    Handshake <$> (A.anyWord8 >>= A.take . fromIntegral)
              <*> A.take 8 <*> A.take 20 <*> A.take 20

handleMessage :: Int -> PeerMessage -> PeerState -> Process PeerState
handleMessage pieceCount m st =
    case m of
        KeepAlive -> return st -- TODO: Send KeepAlive back
        Handshake {} -> return st -- TODO: Verify data
        Choke -> return st { peerChoked = True }
        Unchoke -> return st { peerChoked = False }
        Interested -> return st { peerInterested = True }
        NotInterested -> return st { peerInterested = False }
        Have x -> do
            pid <- getSelfPid
            nsend "db_updater" (pid, PeerHas x)
            return st
        Bitfield x -> do
            let changes = readBitfield pieceCount x
            forM_ changes $ \(x, b) ->
              if b
                then do
                    pid <- getSelfPid
                    nsend "db_updater" (pid, PeerHas x)
                else return ()
            return st

-- TODO: This works (I think), but it's pretty terrible.
readBitfield :: Int -> B.ByteString -> [(Int, Bool)]
readBitfield max = filter (\(i, _) -> i < max) . go 0 . B.unpack
  where
    go n [] = []
    go n (x:xs) = readVals n x ++ go (n + 8) xs
    readVals n x = map (\i -> readVal (n + i) i x) $ reverse [0..7]
    readVal c n x = (c, testBit x n)
