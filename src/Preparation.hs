module Preparation
  ( preparationRecords
  )where

import           Game                           ( Board (..)
                                                , Pos (..)
                                                , Side (..)
                                                , getConfig )
import           Graphic                        ( handlerBoard, posToCoord )
import           Graphics.Gloss.Interface.IO.Game
import           Data.Map.Strict
import           Network.Socket                 ( Socket )
import           Parser                         ( Records(..)
                                                , Result(..)
                                                , position
                                                , positions
                                                , action
                                                , parsePDNFile )
import           Text.ParserCombinators.Parsec  ( Parser
                                                , ParseError
                                                , SourceName
                                                )

getAllBoards :: Result -> (Map Int Board, Board, Int) -> Map Int Board
getAllBoards (Winner side) (views, board, key) =
  insert key (board { Game.winner = Just side }) views
getAllBoards Draw (views, board, key) =
  insert key (board { draw = True }) views
getAllBoards NoInfo (views, board, key) =
  insert key (board { cancelGame = True }) views

getBoards
  :: Socket
  -> [Pos]
  -> Board
  -> Map Int Board
  -> Int
  -> IO (Map Int Board, Board, Int)
getBoards sock (pos : positions) board views key = do
  nextStep <- handlerBoard
    sock
    False
    (EventKey (MouseButton LeftButton)
              Down
              (Modifiers Down Down Down)
              (posToCoord pos)
    )
    board { mySide = activePlayer board } --нужен сокет!
  getBoards sock positions nextStep (insert key nextStep views) (key + 1)
getBoards sock [] board views key = return (views, board, key)

takePositions :: Records -> [Pos]
takePositions records = Prelude.map
  position
  (concatMap positions (Prelude.map action (actions records)))

preparationRecords
  :: Socket -> SourceName -> IO (Either ParseError (Map Int Board))
preparationRecords sock filePath = do
  recordsOrError <- parsePDNFile filePath
  case recordsOrError of
    Left  error   -> return $ Left error
    Right records -> do
      boards <-
        (getBoards sock
                   (takePositions records)
                   getConfig
                   (singleton 1 getConfig)
                   2
        )
      return $ Right $ getAllBoards (Parser.winner records) boards
