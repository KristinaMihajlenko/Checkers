module Main where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.MVar        ( MVar
                                                , newEmptyMVar
                                                , putMVar
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad                  ( forever )
import           Data.Binary                    ( encode
                                                , decode
                                                )
import           Graphic                        ( GameState (..)
                                                , displayGame )
import           Network.Socket          hiding ( recv
                                                , sendAll
                                                )
import           Network.Socket.ByteString.Lazy ( recv
                                                , sendAll
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

main :: IO ()
main = withSocketsDo $ do
  args       <- getArgs
  (ip, port) <- case args of
    ip : port : [] -> return (ip, port)
    _              -> do
      putStrLn "IncorrectArguments"
      putStrLn "Excpected <ip> <port>"
      exitFailure
  addr <- resolve ip port
  bracket (open addr) close runClient

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addrInfo : _ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock

runClient :: Socket -> IO ()
runClient sock = do
  recvMVar <- newEmptyMVar
  forkIO $ recvLoop recvMVar sock
  displayGame (sock, Menu True []) recvMVar

recvLoop :: MVar GameState -> Socket -> IO ()
recvLoop recvMVar sock = do
  forever $ do
    receivedBoard <- fmap decode $ recv sock 2048
    putMVar recvMVar receivedBoard
