{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
module Game
  ( Board (..)
  , Piece (..)
  , Pos (..)
  , Side (..)
  , canAttack
  , findAllDied
  , getConfig
  , otherSide
  , playerToCheckers
  , takeAvailablePos
  ) where

import           Data.Binary       ( Binary )
import           Data.Maybe        ( fromJust
                                   , isNothing
                                   , isJust )
import           GHC.Generics      ( Generic )

type Pos = (Int, Int)
data Piece = Checker | Queen deriving (Eq, Generic, Show)

instance Binary Piece where

data Board = Board { whites          :: [(Pos, Piece)]
                   , blacks          :: [(Pos, Piece)]
                   , selectedChecker :: Maybe (Pos, Piece)
                   , availableMoves  :: [Pos]
                   , activePlayer    :: Side
                   , mySide          :: Side
                   , whiteTimer      :: Float
                   , blackTimer      :: Float
                   , moveMade        :: [Pos]
                   , diedCheckers    :: [Pos]
                   , isAttackLoop    :: Bool
                   , abilityToAttack :: [(Pos, Pos)]
                   , winner          :: Maybe Side
                   , draw            :: Bool
                   , cancelGame      :: Bool
                   , fileName        :: String
                   } deriving (Eq, Generic, Show)

instance Binary Board where

type Player = MoveType -> Board -> Side -> IO Board


playerToCheckers :: Board -> [(Pos, Piece)]
playerToCheckers board =
  if activePlayer board == White then whites board else blacks board

opponentCheckers :: Board -> [(Pos, Piece)]
opponentCheckers board =
  if activePlayer board == White then blacks board else whites board

lastLeftUp :: Pos -> Board -> [Pos] -> (Pos, [Pos])
lastLeftUp (x, y) board acc =
  let lU = helperLeftUp (x, y) board
  in  if
        | null lU   -> ((x, y), acc)
        | otherwise -> lastLeftUp (x - 1, y + 1) board ((head lU) : acc)

lastLeftDown :: Pos -> Board -> [Pos] -> (Pos, [Pos])
lastLeftDown (x, y) board acc =
  let lD = helperLeftDown (x, y) board
  in  if
        | null lD   -> ((x, y), acc)
        | otherwise -> lastLeftDown (x - 1, y - 1) board ((head lD) : acc)

lastRightUp :: Pos -> Board -> [Pos] -> (Pos, [Pos])
lastRightUp (x, y) board acc =
  let rU = helperRightUp (x, y) board
  in  if
        | null rU   -> ((x, y), acc)
        | otherwise -> lastRightUp (x + 1, y + 1) board ((head rU) : acc)

lastRightDown :: Pos -> Board -> [Pos] -> (Pos, [Pos])
lastRightDown (x, y) board acc =
  let rD = helperRightDown (x, y) board
  in  if
        | null rD   -> ((x, y), acc)
        | otherwise -> lastRightDown (x + 1, y - 1) board ((head rD) : acc)

canAttack :: Board -> (Pos, Piece) -> [(Pos, Pos)]
canAttack board (attackPos, Checker) =
  canAttack1 board attackPos
    ++ canAttack2 board attackPos
    ++ canAttack3 board attackPos
    ++ canAttack4 board attackPos
canAttack board (attackPos, Queen) =
  let
    pos = attackPos
    lD  = fst (lastLeftDown attackPos board [])
    cA1 = canAttack1 board lD
    lU  = fst (lastLeftUp attackPos board [])
    cA2 = canAttack2 board lU
    rU  = fst (lastRightUp attackPos board [])
    cA3 = canAttack3 board rU
    rD  = fst (lastRightDown attackPos board [])
    cA4 = canAttack4 board rD
    lDA = if not (null cA1)
      then snd (lastLeftDown (snd (head cA1)) board [])
      else []
    lUA = if not (null cA2)
      then snd (lastLeftUp (snd (head cA2)) board [])
      else []
    rUA = if not (null cA3)
      then snd (lastRightUp (snd (head cA3)) board [])
      else []
    rDA = if not (null cA4)
      then snd (lastRightDown (snd (head cA4)) board [])
      else []
    hcA1 = if not (null cA1) then (attackPos, snd (head cA1)) : [] else []
    hcA2 = if not (null cA2) then (attackPos, snd (head cA2)) : [] else []
    hcA3 = if not (null cA3) then (attackPos, snd (head cA3)) : [] else []
    hcA4 = if not (null cA4) then (attackPos, snd (head cA4)) : [] else []
  in
    hcA1
    ++ hcA2
    ++ hcA3
    ++ hcA4
    ++ (if not (null lDA) then map ((,) pos) lDA else [])
    ++ (if not (null lUA) then map ((,) pos) lUA else [])
    ++ (if not (null rUA) then map ((,) pos) rUA else [])
    ++ (if not (null rDA) then map ((,) pos) rDA else [])

canAttack1 :: Board -> Pos -> [(Pos, Pos)]
canAttack1 board (x, y) =
  if (  isJust (lookup (x - 1, y - 1) (opponentCheckers board))
     && x
     -  2
     >  0
     && y
     -  2
     >  0
     && isNothing (lookup (x - 2, y - 2) (opponentCheckers board))
     && notElem (x - 1, y - 1) (diedCheckers board)
     && isNothing (lookup (x - 2, y - 2) (playerToCheckers board))
     )
    then [((x, y), (x - 2, y - 2))]
    else []

canAttack2 :: Board -> Pos -> [(Pos, Pos)]
canAttack2 board (x, y) =
  if (  isJust (lookup (x - 1, y + 1) (opponentCheckers board))
     && x
     -  2
     >  0
     && y
     +  2
     <  9
     && isNothing (lookup (x - 2, y + 2) (opponentCheckers board))
     && notElem (x - 1, y + 1) (diedCheckers board)
     && isNothing (lookup (x - 2, y + 2) (playerToCheckers board))
     )
    then [((x, y), (x - 2, y + 2))]
    else []

canAttack3 :: Board -> Pos -> [(Pos, Pos)]
canAttack3 board (x, y) =
  if (  isJust (lookup (x + 1, y + 1) (opponentCheckers board))
     && x
     +  2
     <  9
     && y
     +  2
     <  9
     && isNothing (lookup (x + 2, y + 2) (opponentCheckers board))
     && notElem (x + 1, y + 1) (diedCheckers board)
     && isNothing (lookup (x + 2, y + 2) (playerToCheckers board))
     )
    then [((x, y), (x + 2, y + 2))]
    else []

canAttack4 :: Board -> Pos -> [(Pos, Pos)]
canAttack4 board (x, y) =
  if (  isJust (lookup (x + 1, y - 1) (opponentCheckers board))
     && x
     +  2
     <  9
     && y
     -  2
     >  0
     && isNothing (lookup (x + 2, y - 2) (opponentCheckers board))
     && notElem (x + 1, y - 1) (diedCheckers board)
     && isNothing (lookup (x + 2, y - 2) (playerToCheckers board))
     )
    then [((x, y), (x + 2, y - 2))]
    else []

takeAvailablePos :: Board -> (Pos, Piece) -> [Pos]
takeAvailablePos board ((x, y), Checker) =
  (helperRight (x, y) Checker board) ++ helperLeft (x, y) Checker board
takeAvailablePos board ((x, y), Queen) =
  (helperRight (x, y) Queen board) ++ helperLeft (x, y) Queen board

helperRight :: Pos -> Piece -> Board -> [Pos]
helperRight (x, y) Checker board = if (activePlayer board == White)
  then helperRightUp (x, y) board
  else helperRightDown (x, y) board
helperRight (x, y) Queen board =
  let rU = snd (lastRightUp (x, y) board [])
      rD = snd (lastRightDown (x, y) board [])
  in  rU ++ rD

helperRightUp :: Pos -> Board -> [Pos]
helperRightUp (x, y) board =
  if isNothing (lookup (x + 1, y + 1) (whites board))
       && isNothing (lookup (x + 1, y + 1) (blacks board))
       && x
       +  1
       <  9
       && y
       +  1
       <  9
    then (x + 1, y + 1) : []
    else []

helperRightDown :: Pos -> Board -> [Pos]
helperRightDown (x, y) board =
  if isNothing (lookup (x + 1, y - 1) (whites board))
       && isNothing (lookup (x + 1, y - 1) (blacks board))
       && x
       +  1
       <  9
       && y
       -  1
       >  0
    then (x + 1, y - 1) : []
    else []

helperLeft :: Pos -> Piece -> Board -> [Pos]
helperLeft (x, y) Checker board = if (activePlayer board == White)
  then helperLeftUp (x, y) board
  else helperLeftDown (x, y) board
helperLeft (x, y) Queen board =
  let lU = snd (lastLeftUp (x, y) board [])
      lD = snd (lastLeftDown (x, y) board [])
  in  lU ++ lD

helperLeftUp :: Pos -> Board -> [Pos]
helperLeftUp (x, y) board =
  if isNothing (lookup (x - 1, y + 1) (whites board))
       && isNothing (lookup (x - 1, y + 1) (blacks board))
       && x
       -  1
       >  0
       && y
       +  1
       <  9
    then (x - 1, y + 1) : []
    else []

helperLeftDown :: Pos -> Board -> [Pos]
helperLeftDown (x, y) board =
  if isNothing (lookup (x - 1, y - 1) (whites board))
       && isNothing (lookup (x - 1, y - 1) (blacks board))
       && x
       -  1
       >  0
       && y
       -  1
       >  0
    then (x - 1, y - 1) : []
    else []

findAllDied :: Pos -> Board -> Pos
findAllDied pos board =
  if (fst pos > fst ((fst . fromJust) (selectedChecker board)))
    then if (snd pos > snd ((fst . fromJust) (selectedChecker board)))
      then
        let
          (x, y) = fst
            (lastRightUp ((fst . fromJust) (selectedChecker board)) board [])
        in  (x + 1, y + 1)
      else
        let (x, y) =
              fst
                (lastRightDown ((fst . fromJust) (selectedChecker board))
                               board
                               []
                )
        in  (x + 1, y - 1)
    else if (snd pos > snd ((fst . fromJust) (selectedChecker board)))
      then
        let
          (x, y) =
            fst (lastLeftUp ((fst . fromJust) (selectedChecker board)) board [])
        in  (x - 1, y + 1)
      else
        let
          (x, y) = fst
            (lastLeftDown ((fst . fromJust) (selectedChecker board)) board [])
        in  (x - 1, y - 1)

data Side = White | Black deriving (Eq, Generic, Show)

instance Binary Side where

otherSide :: Side -> Side
otherSide White = Black
otherSide Black = White

data MoveType = Move | Attack deriving (Eq, Generic, Show)

instance Binary MoveType where

getConfig :: Board
getConfig = Board
  { whites          = map
                        (flip (,) Checker)
                        [ (1, 1)
                        , (3, 1)
                        , (5, 1)
                        , (7, 1)
                        , (2, 2)
                        , (4, 2)
                        , (6, 2)
                        , (8, 2)
                        , (1, 3)
                        , (3, 3)
                        , (5, 3)
                        , (7, 3)
                        ]
  , blacks          = map
                        (flip (,) Checker)
                        [ (2, 8)
                        , (4, 8)
                        , (6, 8)
                        , (8, 8)
                        , (1, 7)
                        , (3, 7)
                        , (5, 7)
                        , (7, 7)
                        , (2, 6)
                        , (4, 6)
                        , (6, 6)
                        , (8, 6)
                        ]
  , selectedChecker = Nothing
  , availableMoves  = []
  , activePlayer    = White
  , mySide          = White
  , whiteTimer      = 300
  , blackTimer      = 300
  , moveMade        = []
  , diedCheckers    = []
  , isAttackLoop    = False
  , abilityToAttack = []
  , winner          = Nothing
  , draw            = False
  , cancelGame      = False
  , fileName        = ""
  }
