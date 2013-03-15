module BitTorrent.Peer
    ( runPeer
    ) where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import qualified Data.ByteString as B
import Data.Bits
import Data.Char (ord)
import Data.Word
import Network.Socket (Socket)
import qualified Network.Socket as N

import Control.Distributed.Process
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Binary as A
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import BitTorrent.Types

runPeer :: Metainfo -> Peer -> PeerState -> Process ()
runPeer meta peer state = do
    pid <- getSelfPid
    conn <- spawnLocal $ runPeerConnection meta peer pid

    forever $ do
        m <- expect :: Process Message
        say $ "Received message: " ++ show m

runPeerConnection :: Metainfo -> Peer -> ProcessId -> Process ()
runPeerConnection meta peer pid = do
    sock <- liftIO $ peerConnect (peerIp peer) (peerPort peer)
    (is, os) <- liftIO $ Streams.socketToStreams sock

    say $ "Connected to: " ++ show peer

    handshake <- do
        message <- liftIO $ do
            peerHandshake os (mtInfoHash meta)
            peerListen is parseHandshake
        send pid message

    forever $ do
        message <- liftIO $ peerListen is parseMessage
        send pid message

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
        B.concat [ B.pack [protoHeaderSize]
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

peerListen :: Streams.InputStream B.ByteString -> A.Parser Message -> IO Message
peerListen is parser = Streams.parseFromStream parser is

parseMessage :: A.Parser Message
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

parseHandshake :: A.Parser Message
parseHandshake =
    Handshake <$> (A.anyWord8 >>= A.take . fromIntegral)
              <*> A.take 8 <*> A.take 20 <*> A.take 20

handleMessage :: Message -> IO ()
handleMessage m = return ()
{-
    case m of
        KeepAlive -> return () -- TODO: Send KeepAlive back
        Handshake {} -> modify $ \s -> s { peerHandshaken = True }
        Choke -> modify $ \s -> s { peerChoked = True }
        Unchoke -> modify $ \s -> s { peerChoked = False }
        Interested -> modify $ \s -> s { peerInterested = True }
        NotInterested -> modify $ \s -> s { peerInterested = False }
        Have x -> modify $ \s -> s { peerHas = peerHas s // [(x, True)] }
        Bitfield x -> do
            pieceCount <- length . mtPieces <$> ask
            let changes = readBitfield pieceCount x
            modify $ \s -> s { peerHas = peerHas s // changes }
        _ -> error $ "Unimplemented message handling of " ++ show m
-}

readBitfield :: Int -> B.ByteString -> [(Int, Bool)]
readBitfield max = filter (\(i, _) -> i <= max) . go 0 . B.unpack
  where
    go n [] = []
    go n (x:xs) = readVals n x ++ go (n + 8) xs
    readVals n x = map (\i -> readVal (n + i) x) $ reverse [0..7]
    readVal n x = (n, testBit x n)
