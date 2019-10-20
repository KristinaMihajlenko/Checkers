{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphic
  ( GameState (..)
  , displayGame
  , handlerBoard
  , posToCoord
  ) where

import           Control.Concurrent.MVar        ( MVar
                                                , tryTakeMVar )
import           Control.Monad                  ( when )
import           Data.Binary                    ( Binary
                                                , encode
                                                , decode )
import           Data.Char                      ( isDigit )
import           Data.List.Utils                ( delFromAL )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                , fromMaybe )
import           Game                           ( Board (..)
                                                , Pos (..)
                                                , Piece (..)
                                                , Side (..)
                                                , otherSide
                                                , takeAvailablePos
                                                , playerToCheckers
                                                , canAttack
                                                , findAllDied )
import           GHC.Generics                   ( Generic )
import           Graphics.Gloss.Data.Bitmap     ( loadBMP )
import           Graphics.Gloss.Data.Point      ( pointInBox )
import           Graphics.Gloss.Interface.IO.Game
import           Network.Socket                 ( Socket )
import           Network.Socket.ByteString.Lazy ( sendAll )
import           System.Exit                    ( exitSuccess )

size, pos :: (Int, Int)
size = (800, 600)
pos = (0, 0)

rect :: Picture
rect = polygon $ rectanglePath 50 50

backgroundColor, darkColor :: Color
backgroundColor = makeColor (164 / 255) (132 / 255) (100 / 255) 1
lightColor = makeColor (231 / 255) (207 / 255) (169 / 255) 1
darkColor = makeColor (124 / 255) (98 / 255) (73 / 255) 1

data GameState
  = Menu { needSend :: Bool, files :: [String] }
  | Game Board
  | Watch Board
    deriving (Eq, Generic, Show)

instance Binary GameState where

displayGame :: (Socket, GameState) -> MVar GameState -> IO ()
displayGame (sock, gameState) recvMVar = do
  blackChecker <- fmap ((,) "blackChecker") $ loadBMP "images/blackChecker.bmp"
  blackQueen   <- fmap ((,) "blackQueen") $ loadBMP "images/blackQueen.bmp"
  whiteChecker <- fmap ((,) "whiteChecker") $ loadBMP "images/whiteChecker.bmp"
  whiteQueen   <- fmap ((,) "whiteQueen") $ loadBMP "images/whiteQueen.bmp"
  let images =
        Map.fromList [blackChecker, blackQueen, whiteChecker, whiteQueen]
  playIO (InWindow "Checkers" size pos)
         backgroundColor
         10
         gameState
         (renderer images)
         (handler sock)
         (updater sock recvMVar)

renderer :: Map.Map String Picture -> GameState -> IO Picture
renderer images (Game  board) = renderBoard images board
renderer images (Watch board) = renderWatcher images board
renderer _      _             = renderMenu

renderWatcher :: Map.Map String Picture -> Board -> IO Picture
renderWatcher images board = do
  let rects     = pictures (getRects 1 1 [])
  let border    = lineLoop $ rectanglePath 400 400
  let outBorder = lineLoop $ rectanglePath 440 440
  let numbers   = map (scale 0.1 0.1 . text . show) [1 .. 8]
  let numberPic = pictures
        (zipWith ($) (map (translate (-213)) [(-180), (-130) .. 170]) numbers)
  let chars = map (scale 0.1 0.1 . text . (: [])) ['a' .. 'h']
  let charPic = pictures
        (zipWith ($) (map (flip translate (-213)) [(-180), (-130) .. 170]) chars
        )
  let whiteCheckers = getWhiteCheckers images board
  let blackCheckers = getBlackCheckers images board
  let selectLine    = getSelectLine board
  let availableLine = getAvailableLine board
  let desk = color lightColor (polygon $ rectanglePath 440 440)
  let sideOfPlayer =
        translate (-30) 280
          . color black
          . scale 0.15 0.15
          . text
          $ if (White == mySide board) then "WHITE" else "BLACK"
  let whoseTurn =
        translate (-70) 240
          . color black
          . scale 0.15 0.15
          . text
          $ (show (activePlayer board) ++ " TURN")
  let lastMove = getLastMoveLine board
  let winPicture = case (winner board) of
        Nothing   -> blank
        Just side -> getWinnerMessage board
  let drawPicture = case (draw board) of
        False -> blank
        True  -> getDrawMessage board
  let cancelPicture = case (cancelGame board) of
        False -> blank
        True  -> getCancelMessage board
  return $ pictures
    ([ desk
     , border
     , outBorder
     , selectLine
     , availableLine
     , whoseTurn
     , lastMove
     , rects
     , numberPic
     , charPic
     , whiteCheckers
     , blackCheckers
     , sideOfPlayer
     , leftButton
     , rightButton
     , winPicture
     , drawPicture
     , cancelPicture
     ]
    )

renderMenu :: IO Picture
renderMenu = do
  let border = lineLoop $ rectanglePath 400 400
  let newGame = pictures
        ([ (translate 0 0) $ (lineLoop $ rectanglePath 200 60)
         , ( translate (-60) 115
           . color black
           . scale 0.15 0.15
           . text
           $ "NEW GAME"
           )
         ]
        )
  let continueGame = pictures
        ([ (translate 0 115) $ (lineLoop $ rectanglePath 200 60)
         , ( translate (-80) 0
           . color black
           . scale 0.15 0.15
           . text
           $ "WATCH GAME"
           )
         ]
        )
  let exit = pictures
        ([ (translate 0 (-115)) $ (lineLoop $ rectanglePath 200 60)
         , ( translate (-30) (-115)
           . color black
           . scale 0.15 0.15
           . text
           $ "EXIT"
           )
         ]
        )
  return $ pictures ([border, newGame, continueGame, exit])

renderBoard :: Map.Map String Picture -> Board -> IO Picture
renderBoard images board = do
  let rects     = pictures (getRects 1 1 [])
  let border    = lineLoop $ rectanglePath 400 400
  let outBorder = lineLoop $ rectanglePath 440 440
  let numbers   = map (scale 0.1 0.1 . text . show) [1 .. 8]
  let numberPic = pictures
        (zipWith ($) (map (translate (-213)) [(-180), (-130) .. 170]) numbers)
  let chars = map (scale 0.1 0.1 . text . (: [])) ['a' .. 'h']
  let charPic = pictures
        (zipWith ($) (map (flip translate (-213)) [(-180), (-130) .. 170]) chars
        )
  let whiteCheckers = getWhiteCheckers images board
  let blackCheckers = getBlackCheckers images board
  let selectLine    = getSelectLine board
  let availableLine = getAvailableLine board
  let whiteTimerPic =
        translate (-325) 80
          . color black
          . scale 0.1 0.1
          . text
          . showTimer
          . round
          . whiteTimer
          $ board
  let blackTimerPic =
        translate 300 80
          . color black
          . scale 0.1 0.1
          . text
          . showTimer
          . round
          . blackTimer
          $ board
  let desk = color lightColor (polygon $ rectanglePath 440 440)
  let sideOfPlayer =
        translate (-30) 280
          . color black
          . scale 0.15 0.15
          . text
          $ if (White == mySide board) then "WHITE" else "BLACK"
  let
    whoseTurn =
      translate (-70) 240
        . color black
        . scale 0.15 0.15
        . text
        $ if (activePlayer board == mySide board)
            then " YOUR TURN"
            else "ENEMY'S TURN"
  let lastMove     = getLastMoveLine board
  let giveUpButton = giveUp
  let winPicture = case (winner board) of
        Nothing   -> blank
        Just side -> getWinnerMessage board
  let drawPicture = case (draw board) of
        False -> blank
        True  -> getDrawMessage board
  let cancelPicture = case (cancelGame board) of
        False -> blank
        True  -> getCancelMessage board
  return $ pictures
    ([ desk
     , border
     , outBorder
     , selectLine
     , availableLine
     , whoseTurn
     , whiteTimerPic
     , blackTimerPic
     , lastMove
     , rects
     , numberPic
     , charPic
     , whiteCheckers
     , blackCheckers
     , sideOfPlayer
     , giveUpButton
     , winPicture
     , drawPicture
     , cancelPicture
     ]
    )

leftButton :: Picture
leftButton = pictures
  ([ (translate (-200) (-250)) $ (lineLoop $ rectanglePath 100 30)
   , translate (-225) (-255) . color black . scale 0.1 0.1 . text $ "Left"
   ]
  )

rightButton :: Picture
rightButton = pictures
  ([ (translate (200) (-250)) $ (lineLoop $ rectanglePath 100 30)
   , translate (225) (-255) . color black . scale 0.1 0.1 . text $ "Right"
   ]
  )

giveUp :: Picture
giveUp = pictures
  ([ (translate (-200) (-250)) $ (lineLoop $ rectanglePath 100 30)
   , (translate (-225) (-255) . color black . scale 0.1 0.1 . text $ "Give Up")
   ]
  )

getCancelMessage :: Board -> Picture
getCancelMessage board = pictures
  ([ getFinalMessage board
   , ((translate (-45) 30 . color black . scale 0.15 0.15 . text) $ "CANCEL")
   ]
  )


getDrawMessage :: Board -> Picture
getDrawMessage board = pictures
  ([ getFinalMessage board
   , ((translate (-30) 30 . color black . scale 0.15 0.15 . text) $ "DRAW")
   ]
  )

getWinnerMessage :: Board -> Picture
getWinnerMessage board = pictures
  ([ getFinalMessage board
   , ( (translate (-60) 30 . color black . scale 0.15 0.15 . text)
     $ if fromJust (winner board) == mySide board
         then " YOU WIN!"
         else "YOU LOSE!"
     )
   ]
  )

getFinalMessage :: Board -> Picture
getFinalMessage board = pictures
  ([ (color (makeColor 1 1 1 0.5) . polygon $ rectanglePath 800 600)
   , (color lightColor (polygon $ rectanglePath 300 150))
   , (lineLoop $ rectanglePath 300 150)
   , ((translate (-75) (-35)) $ (lineLoop $ rectanglePath 100 30))
   , ((translate (75) (-35)) $ (lineLoop $ rectanglePath 100 30))
   , ( translate (-50) (5)
     . color black
     . scale 0.1 0.1
     . text
     $ (fileName board)
     )
   , (translate (-107) (-37) . color black . scale 0.1 0.1 . text $ "menu")
   , (translate (40) (-37) . color black . scale 0.1 0.1 . text $ "exit")
   ]
  )

showTimer :: Int -> String
showTimer x = show (x `div` 60) ++ ":" ++ if x `mod` 60 < 10
  then '0' : show (x `mod` 60)
  else show (x `mod` 60)

posToCoord :: Pos -> Point
posToCoord (x, y) =
  (fromIntegral $ (-175) + (x - 1) * 50, fromIntegral $ (-175) + (y - 1) * 50)

coordToPos :: Point -> Pos
coordToPos (x, y) =
  ((round x + 200) `div` 50 + 1, (round y + 200) `div` 50 + 1)

getWhiteCheckers :: Map.Map String Picture -> Board -> Picture
getWhiteCheckers images board =
  let checker = images Map.! "whiteChecker"
      queen   = images Map.! "whiteQueen"
  in  pictures (map (helper checker queen) $ whites board)

getBlackCheckers :: Map.Map String Picture -> Board -> Picture
getBlackCheckers images board =
  let checker = images Map.! "blackChecker"
      queen   = images Map.! "blackQueen"
  in  pictures (map (helper checker queen) $ blacks board)

helper :: Picture -> Picture -> (Pos, Piece) -> Picture
helper checker _ (coord, Checker) =
  uncurry translate (posToCoord coord) checker
helper _ queen (coord, Queen) = uncurry translate (posToCoord coord) queen

getRects :: Int -> Int -> [Picture] -> [Picture]
getRects x y acc = if x > 8
  then if y == 8
    then acc
    else getRects (if y `mod` 2 == 0 then 1 else 2) (y + 1) acc
  else
    let (xx, yy) = posToCoord (x, y)
    in  getRects (x + 2) y (color darkColor (translate xx yy rect) : acc)

getLastMoveLine :: Board -> Picture
getLastMoveLine board = if (null (moveMade board))
  then blank
  else pictures $ concatMap
    (\pos -> map (color red . uncurry translate (posToCoord pos) . lineLoop)
                 [rectanglePath 51 51, rectanglePath 53 53]
    )
    (moveMade board)

getSelectLine :: Board -> Picture
getSelectLine board = case selectedChecker board of
  Nothing        -> blank
  Just (pos, ch) -> pictures $ map
    (color red . uncurry translate (posToCoord pos) . lineLoop)
    [rectanglePath 51 51, rectanglePath 53 53]

getAvailableLine :: Board -> Picture
getAvailableLine board = pictures $ concatMap
  (\pos -> map (color green . uncurry translate (posToCoord pos) . lineLoop)
               [rectanglePath 51 51, rectanglePath 53 53]
  )
  (availableMoves board)

handler :: Socket -> Event -> GameState -> IO GameState
handler sock event (Game board) =
  fmap Game (handlerBoard sock True event board)
handler sock event (Watch board) = fmap Watch (handlerWatcher sock event board)
handler sock event menu          = handlerMenu menu sock event

handlerWatcher :: Socket -> Event -> Board -> IO Board
handlerWatcher sock (EventKey (MouseButton LeftButton) Down _ point) board = do
  if (pointInBox point (-100, -265) (-300, -235))
    then do
      sendAll sock (encode "-1")
      return board
    else do
      if (pointInBox point (300, -265) (100, -235))
        then do
          sendAll sock (encode "1")
          return board
        else do
          if (isJust (winner board)) || (draw board) || (cancelGame board)
            then do
              if (pointInBox point (-25, -50) (-125, -20))
                then do
                  sendAll sock (encode "Menu")
                  return board
                else do
                  if (pointInBox point (125, -50) (25, -20))
                    then do
                      sendAll sock (encode "Exit")
                      exitSuccess
                    else return board
            else return board
handlerWatcher _ _ board = return board

getFileNum :: Int -> IO Int
getFileNum size = do
  fileNum <- getLine
  if all isDigit fileNum && (read fileNum) <= size && (read fileNum) > 0
    then return (read fileNum - 1)
    else do
      putStrLn "Invalid number"
      getFileNum size

handlerMenu :: GameState -> Socket -> Event -> IO GameState
handlerMenu menu sock (EventKey (MouseButton LeftButton) Down _ point) = do
  if (pointInBox point (100, 85) (-100, 145))
    then do
      when (needSend menu) $ sendAll sock (encode "New Game")
      return menu { needSend = False }
    else do
      if (pointInBox point (100, -30) (-100, 30))
        then do
          when (needSend menu) $ sendAll sock (encode "Watch Game")
          return menu { needSend = False }
        else do
          if (pointInBox point (100, -145) (-100, -85))
            then do
              when (needSend menu) $ sendAll sock (encode "Exit")
              exitSuccess
            else return menu
handlerMenu menu sock _ = return menu

handlerBoard :: Socket -> Bool -> Event -> Board -> IO Board
handlerBoard sock useSock (EventKey (MouseButton LeftButton) Down _ point) board
  = do
    if activePlayer board /= mySide board && isNothing (winner board)
      then return board
      else do
        if isJust (winner board)
          then do
            if (pointInBox point (-25, -50) (-125, -20))
              then do
                when useSock $ sendAll sock (encode "Menu")
                return board
              else do
                if (pointInBox point (125, -50) (25, -20))
                  then do
                    when useSock $ sendAll sock (encode "Exit")
                    exitSuccess
                  else return board
          else do
            if (pointInBox point (-150, -265) (-250, -235))
              then do
                let newBoard =
                      board { winner = Just $ otherSide (activePlayer board) }
                when useSock $ sendAll sock (encode newBoard)
                return newBoard
              else do
                let pos = coordToPos point
                if (isAttackLoop board)
                  then do
                    if elem pos (availableMoves board)
                      then do
                        let died = findAllDied pos board
                        let newAttacks = canAttack
                              board { diedCheckers = died : (diedCheckers board)
                                    }
                              (pos, checkerOrQueen board pos)
                        if (null newAttacks)
                          then do
                            let
                              newBoard = board
                                { whites          = changeDesk
                                                      pos
                                                      White
                                                      board
                                                      (activePlayer board)
                                                      (died : (diedCheckers board))
                                                      (whites board)
                                , blacks          = changeDesk
                                                      pos
                                                      Black
                                                      board
                                                      (activePlayer board)
                                                      (died : (diedCheckers board))
                                                      (blacks board)
                                , activePlayer = otherSide (activePlayer board)
                                , moveMade        = pos : (moveMade board)
                                , selectedChecker = Nothing
                                , availableMoves  = []
                                , isAttackLoop    = False
                                , abilityToAttack = []
                                }

                            when useSock $ sendAll sock (encode newBoard)
                            return newBoard
                          else do
                            let
                              newBoard = board
                                { whites = changeDesk pos
                                                      White
                                                      board
                                                      (activePlayer board)
                                                      []
                                                      (whites board)
                                , blacks = changeDesk pos
                                                      Black
                                                      board
                                                      (activePlayer board)
                                                      []
                                                      (blacks board)
                                , moveMade        = pos : (moveMade board)
                                , selectedChecker =
                                  Just (pos, checkerOrQueen board pos)
                                , availableMoves  = map snd newAttacks
                                , isAttackLoop    = True
                                , diedCheckers    = died : (diedCheckers board)
                                }
                            when useSock $ sendAll sock (encode newBoard)
                            return newBoard
                      else do
                        return board
                  else do
                    let attacks = if (null (abilityToAttack board))
                          then concatMap (canAttack board)
                                         (playerToCheckers board)
                          else (abilityToAttack board)
                    if (not (null attacks))
                      then do
                        if (  isJust (lookup pos (whites board))
                           || isJust (lookup pos (blacks board))
                           )
                        then
                          do
                            case lookup pos (playerToCheckers board) of
                              Nothing  -> return board
                              Just chP -> case lookup pos attacks of
                                Nothing -> return $ board
                                  { selectedChecker = Just $ (pos, chP)
                                  , abilityToAttack = attacks
                                  , moveMade        = []
                                  , availableMoves  = []
                                  }
                                Just ch -> return $ board
                                  { selectedChecker = Just $ (pos, chP)
                                  , abilityToAttack = attacks
                                  , availableMoves  = map
                                                        snd
                                                        (canAttack board
                                                                   (pos, chP)
                                                        )
                                  , moveMade        = []
                                  }
                        else
                          do
                            if elem pos (availableMoves board)
                              then do
                                let died = findAllDied pos board
                                let
                                  newAttacks = canAttack
                                    board
                                      { diedCheckers = died
                                                         : (diedCheckers board)
                                      }
                                    (pos, checkerOrQueen board pos)

                                if (null newAttacks)
                                  then do
                                    let
                                      newBoard = board
                                        { whites          = changeDesk
                                                              pos
                                                              White
                                                              board
                                                              (activePlayer board)
                                                              (died : [])
                                                              (whites board)
                                        , blacks          = changeDesk
                                                              pos
                                                              Black
                                                              board
                                                              (activePlayer board)
                                                              (died : [])
                                                              (blacks board)
                                        , activePlayer    = otherSide
                                                              (activePlayer board)
                                        , moveMade        =
                                          pos
                                          : (fst
                                              (fromJust (selectedChecker board))
                                            )
                                          : []
                                        , selectedChecker = Nothing
                                        , availableMoves  = []
                                        , abilityToAttack = []
                                        , diedCheckers    = died : []
                                        }
                                    when useSock
                                      $ sendAll sock (encode newBoard)
                                    return newBoard
                                  else do
                                    let
                                      newBoard = board
                                        { whites          = changeDesk
                                                              pos
                                                              White
                                                              board
                                                              (activePlayer board)
                                                              []
                                                              (whites board)
                                        , blacks          = changeDesk
                                                              pos
                                                              Black
                                                              board
                                                              (activePlayer board)
                                                              []
                                                              (blacks board)
                                        , moveMade        =
                                          pos
                                          : (fst
                                              (fromJust (selectedChecker board))
                                            )
                                          : []
                                        , selectedChecker =
                                          Just (pos, checkerOrQueen board pos)
                                        , availableMoves  = map snd newAttacks
                                        , isAttackLoop    = True
                                        , diedCheckers    = died : []
                                        , abilityToAttack = []
                                        }
                                    when useSock
                                      $ sendAll sock (encode newBoard)
                                    return newBoard
                              else return board
                      else do
                        if (  isJust (lookup pos (whites board))
                           || isJust (lookup pos (blacks board))
                           )
                        then
                          do
                            case lookup pos (playerToCheckers board) of
                              Nothing -> return board
                              Just ch -> return $ board
                                { selectedChecker = Just
                                                      $ (coordToPos point, ch)
                                , availableMoves  = takeAvailablePos board
                                                                     (pos, ch)
                                , moveMade        = []
                                }
                        else
                          do
                            if elem pos (availableMoves board)
                              then do
                                let
                                  newBoard = board
                                    { whites = changeDesk pos
                                                          White
                                                          board
                                                          (activePlayer board)
                                                          []
                                                          (whites board)
                                    , blacks = changeDesk pos
                                                          Black
                                                          board
                                                          (activePlayer board)
                                                          []
                                                          (blacks board)
                                    , activePlayer = otherSide
                                                       (activePlayer board)
                                    , moveMade =
                                      pos
                                      : (fst (fromJust (selectedChecker board)))
                                      : []
                                    , selectedChecker = Nothing
                                    , availableMoves = []
                                    }
                                when useSock $ sendAll sock (encode newBoard)
                                return newBoard
                              else return board
handlerBoard _ _ _ board = return board

changeDesk
  :: Pos -> Side -> Board -> Side -> [Pos] -> [(Pos, Piece)] -> [(Pos, Piece)]
changeDesk pos White board White diedCh w =
  ( pos
    , if (snd pos == 8 || snd (fromJust (selectedChecker board)) == Queen)
      then Queen
      else Checker
    )
    : delFromAL (whites board) (fst (fromJust (selectedChecker board)))
changeDesk pos White board Black [] w = w
changeDesk pos White board Black (ch : died) w =
  changeDesk pos White board Black died (delFromAL w ch)
changeDesk pos Black board Black diedCh b =
  ( pos
    , if (snd pos == 1 || snd (fromJust (selectedChecker board)) == Queen)
      then Queen
      else Checker
    )
    : delFromAL (blacks board) (fst (fromJust (selectedChecker board)))
changeDesk pos Black board White [] b = b
changeDesk pos Black board White (ch : died) b =
  changeDesk pos Black board White died (delFromAL b ch)

checkerOrQueen :: Board -> Pos -> Piece
checkerOrQueen board (x, y) =
  if (  (activePlayer board)
     == White
     && y
     == 8
     || (activePlayer board)
     == Black
     && y
     == 1
     )
    then Queen
    else snd (fromJust (selectedChecker board))

updater :: Socket -> MVar GameState -> Float -> GameState -> IO GameState
updater sock recvMVar time (Game board) = updaterBoard sock recvMVar time board
updater sock recvMVar time (Watch board) =
  updaterWatcher sock recvMVar time board
updater sock recvMVar time menu = updaterMenu sock recvMVar time menu

updaterWatcher :: Socket -> MVar GameState -> Float -> Board -> IO GameState
updaterWatcher sock recvMVar time board = do
  receivedBoard <- tryTakeMVar recvMVar
  case receivedBoard of
    Nothing -> return (Watch board)
    Just gs -> return gs

updaterMenu :: Socket -> MVar GameState -> Float -> GameState -> IO GameState
updaterMenu sock recvMVar time menu = do
  receivedBoard <- tryTakeMVar recvMVar
  case receivedBoard of
    Nothing -> return menu
    Just gs -> do
      case gs of
        gs@(Menu _ []   ) -> return gs
        gs@(Menu _ saves) -> do
          let fileList = zip [1 ..] saves
          mapM_ putStrLn (map (\(i, f) -> show i ++ " " ++ f) fileList)
          num <- getFileNum (length saves)
          sendAll sock (encode (saves !! num))
          return (gs { files = [] })
        gs -> return gs

updaterBoard :: Socket -> MVar GameState -> Float -> Board -> IO GameState
updaterBoard sock recvMVar time board = do
  receivedBoard <- tryTakeMVar recvMVar
  updatedTimer  <- if isJust (winner board)
    then return board
    else do
      if activePlayer board == White
        then if whiteTimer board - time < 0
          then do
            let newBoard =
                  board { winner = Just $ otherSide (activePlayer board) }
            sendAll sock (encode newBoard)
            return newBoard
          else return board { whiteTimer = whiteTimer board - time }
        else if blackTimer board - time < 0
          then do
            let newBoard =
                  board { winner = Just $ otherSide (activePlayer board) }
            sendAll sock (encode newBoard)
            return newBoard
          else return board { blackTimer = blackTimer board - time }
  return $ fromMaybe (Game updatedTimer) receivedBoard
