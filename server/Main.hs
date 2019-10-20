{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Async       ( waitBoth
                                                , async )
import           Control.Concurrent.MVar        ( MVar
                                                , newEmptyMVar
                                                , putMVar
                                                , takeMVar
                                                , tryPutMVar
                                                , readMVar
                                                )
import           Control.Exception              ( bracket )
import           Control.Monad                  ( when
                                                , forever
                                                )
import           Data.Binary                    ( encode
                                                , decode
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( isNothing
                                                , isJust
                                                , fromJust )
import           Game                           ( Board (..)
                                                , Pos (..)
                                                , Side (..)
                                                , otherSide
                                                , playerToCheckers
                                                , takeAvailablePos
                                                , canAttack
                                                , getConfig )
import           Graphic                        ( GameState (..) )
import           Network.Socket          hiding ( recv
                                                , sendAll
                                                )
import           Network.Socket.ByteString.Lazy ( recv
                                                , sendAll
                                                )
import           Preparation                    ( preparationRecords )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           System.Directory               ( listDirectory )
import           System.Time                    ( ClockTime(TOD)
                                                , getClockTime
                                                )

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  port <- case args of
    [port] -> return port
    _      -> do
      putStrLn "IncorrectArguments"
      putStrLn "Excpected <port> <playersCount>"
      exitFailure
  let initialBoard = getConfig
  addr  <- resolve port
  mVar1 <- newEmptyMVar
  mVar2 <- newEmptyMVar
  bracket (open addr) close (runServer mVar1 mVar2 initialBoard)

resolve :: String -> IO AddrInfo
resolve port = do
  let hints =
        defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 2
  bind sock (addrAddress addr)
  listen sock 2
  return sock

recvMessage :: Socket -> IO String
recvMessage socket = fmap decode $ recv socket 2048

haveAgreed :: Socket -> Socket -> IO String
haveAgreed clientSocket1 clientSocket2 = do
  smth1 <- async (recvMessage clientSocket1)
  smth2 <- async (recvMessage clientSocket2)
  tmp   <- waitBoth smth1 smth2
  let first  = (fst tmp)
  let second = (snd tmp)
  if (first == second)
    then return (fst tmp)
    else (haveAgreed clientSocket1 clientSocket2)

playCheckers :: MVar Socket -> MVar Socket -> Board -> IO ()
playCheckers clientMVar1 clientMVar2 board = do
  clientSocket1 <- readMVar clientMVar1
  clientSocket2 <- takeMVar clientMVar2
  _             <- takeMVar clientMVar1
  (TOD time _)  <- getClockTime
  let name     = show time
  let filePath = "saves\\" ++ show time ++ ".pdn"
  sendAll clientSocket1
          (encode (Game board { mySide = White, fileName = name }))
  sendAll clientSocket2
          (encode (Game board { mySide = Black, fileName = name }))
  forkIO $ playCheckers clientMVar1 clientMVar2 board
  loop filePath clientSocket1 clientSocket2 board
  forkIO $ handleMessage clientMVar1 clientMVar2 clientSocket1
  forkIO $ handleMessage clientMVar1 clientMVar2 clientSocket2
  return ()

handleMessage :: MVar Socket -> MVar Socket -> Socket -> IO ()
handleMessage mVar1 mVar2 socket = do
  message <- recvMessage socket
  when (message == "Menu") $ do
    sendAll socket (encode (Menu True []))
    isPlay <- handleMenu mVar1 mVar2 socket
    when isPlay $ tryConnect mVar1 mVar2 socket

tryConnect :: MVar Socket -> MVar Socket -> Socket -> IO ()
tryConnect mVar1 mVar2 sock = do
  var1 <- tryPutMVar mVar1 sock
  when (not var1) $ do
    var2 <- tryPutMVar mVar2 sock
    when (not var2) $ tryConnect mVar1 mVar2 sock

runServer :: MVar Socket -> MVar Socket -> Board -> Socket -> IO ()
runServer clientMVar1 clientMVar2 board sock = do
  forkIO $ playCheckers clientMVar1 clientMVar2 board
  forever $ do
    waitClient clientMVar1 clientMVar2 sock

handleMenu :: MVar Socket -> MVar Socket -> Socket -> IO Bool
handleMenu mVar1 mVar2 clientSock = do
  message <- recvMessage clientSock
  if (message == "New Game")
    then return True
    else if (message == "Watch Game")
      then do
        saves <- listDirectory "saves"
        sendAll clientSock (encode (Menu False saves))
        filePath <- recvMessage clientSock
        tryGetViews <- preparationRecords clientSock ("saves\\" ++ filePath)
        case tryGetViews of
          Left error -> do
            putStrLn $ show error
            return False
          Right views -> do
            forkIO $ watcherClient mVar1 mVar2 views 1 clientSock
            return False
      else return False

waitClient :: MVar Socket -> MVar Socket -> Socket -> IO ()
waitClient mVar1 mVar2 sock = do
  (clientSock, _) <- accept sock
  isPlay          <- handleMenu mVar1 mVar2 clientSock
  if isPlay
    then tryConnect mVar1 mVar2 clientSock
    else waitClient mVar1 mVar2 sock

watcherClient
  :: MVar Socket -> MVar Socket -> M.Map Int Board -> Int -> Socket -> IO ()
watcherClient mVar1 mVar2 views key sock = do
  sendAll sock (encode (Watch (fromJust (M.lookup key views))))
  watcherLoop mVar1 mVar2 views key sock (M.size views)

watcherLoop
  :: MVar Socket
  -> MVar Socket
  -> M.Map Int Board
  -> Int
  -> Socket
  -> Int
  -> IO ()
watcherLoop mVar1 mVar2 views key sock size = do
  mess <- fmap decode (recv sock 2048)
  if mess == "Menu"
    then do
      sendAll sock (encode (Menu True []))
      isPlay <- handleMenu mVar1 mVar2 sock
      when isPlay $ tryConnect mVar1 mVar2 sock
    else if mess == "Exit"
      then return ()
      else do
        let change = read mess
        if (key + change == 0 || key + change == size + 1)
          then do
            sendAll sock (encode (Watch (fromJust (M.lookup key views))))
            watcherLoop mVar1 mVar2 views key sock size
          else do
            sendAll
              sock
              (encode (Watch (fromJust (M.lookup (key + change) views))))
            watcherLoop mVar1 mVar2 views (key + change) sock size

loop :: FilePath -> Socket -> Socket -> Board -> IO ()
loop filePath clientSocket1 clientSocket2 board = do
  newBoard <- loop1 filePath clientSocket1 clientSocket2 board
  if isNothing (winner newBoard)
    then do
      let winnerSide = checkWinner newBoard
      case winnerSide of
        Nothing -> loop filePath
                        clientSocket1
                        clientSocket2
                        newBoard { winner = winnerSide }
        Just White -> do
          sendAll clientSocket1 (encode (Game newBoard { winner = winnerSide }))
          sendAll clientSocket2 (encode (Game newBoard { winner = winnerSide }))
          appendFile filePath "2-0"
        Just Black -> do
          sendAll clientSocket1 (encode (Game newBoard { winner = winnerSide }))
          sendAll clientSocket2 (encode (Game newBoard { winner = winnerSide }))
          appendFile filePath "0-2"
    else return ()

checkWinner :: Board -> Maybe Side
checkWinner board =
  if (  isJust (winner board)
     || null (playerToCheckers board)
     || (null (concatMap (canAttack board) (playerToCheckers board)))
     && (null (concatMap (takeAvailablePos board) (playerToCheckers board)))
     )
  then
    Just (otherSide (activePlayer board))
  else
    Nothing

loop1 :: FilePath -> Socket -> Socket -> Board -> IO Board
loop1 filePath clientSocket1 clientSocket2 board = do
  newBoard <- if activePlayer board == White
    then do
      let r = recv clientSocket1 2048
      fmap decode (r)
    else do
      fmap decode (recv clientSocket2 2048)
  if activePlayer board == White
    then do
      if (isNothing (winner newBoard))
        then do
          appendFile filePath "1. "
          writeToFile filePath newBoard
        else do
          if ((winner newBoard) == Just White)
            then appendFile filePath "2-0"
            else appendFile filePath "0-2"
      sendAll clientSocket2
              (encode (Game newBoard { mySide = Black, diedCheckers = [] }))
    else do
      if (isNothing (winner newBoard))
        then writeToFile filePath newBoard
        else do
          if ((winner newBoard) == Just White)
            then appendFile filePath "2-0"
            else appendFile filePath "0-2"
      sendAll clientSocket1
              (encode (Game newBoard { mySide = White, diedCheckers = [] }))
  if activePlayer board == activePlayer newBoard && isNothing (winner newBoard)
    then loop1 filePath clientSocket1 clientSocket2 newBoard
    else return newBoard

writeToFile :: FilePath -> Board -> IO ()
writeToFile filePath board = do
  if (null (diedCheckers board))
    then appendFile
      filePath
      (  posToNormal (last (moveMade board))
      ++ "-"
      ++ posToNormal (head (moveMade board))
      ++ " "
      )
    else do
      let endOfAttack = head (moveMade board)
      let normalOrder = reverse (tail (moveMade board))
      mapM_ (appendFile filePath)
            (map (\x -> posToNormal x ++ "x") (normalOrder))
      appendFile filePath (posToNormal endOfAttack ++ " ")

posToNormal :: Pos -> String
posToNormal (l, d) | l == 1 = 'a' : show d
                   | l == 2 = 'b' : show d
                   | l == 3 = 'c' : show d
                   | l == 4 = 'd' : show d
                   | l == 5 = 'e' : show d
                   | l == 6 = 'f' : show d
                   | l == 7 = 'g' : show d
                   | l == 8 = 'h' : show d
