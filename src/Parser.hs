module Parser
  ( Records(..)
  , Result(..)
  , action
  , parsePDNFile
  , position
  , positions
  ) where


import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( digitToInt )
import           Game                           ( Side(..)
                                                , Pos(..)
                                                )
import           Text.ParserCombinators.Parsec  ( Parser
                                                , ParseError
                                                , char
                                                , string
                                                , letter
                                                , digit
                                                , many
                                                , space
                                                , eof
                                                , many1
                                                , oneOf
                                                , try
                                                , parse
                                                , parseFromFile
                                                )

data Records = Records
  { actions :: [Record]
  , winner  :: Result
  }
data Record = Record { action :: Action }

data Result = Winner Side | Draw | NoInfo

data Action = Action { positions  :: [Position] }

data Position = Position { position :: Pos }

parsePDNFile :: FilePath -> IO (Either ParseError Records)
parsePDNFile = parseFromFile recordsParser

recordsParser :: Parser Records
recordsParser = do
  actions <- try $ many $ recordParser
  winner  <- resultParser
  return Records { actions = actions, winner = winner }

recordParser :: Parser Record
recordParser = do
  action <-
    try
    $   many1 digit
    *>  char '.'
    *>  space
    *>  actionParser
    <*  space
    <|> actionParser
    <*  space
  return Record { action = action }


actionParser :: Parser Action
actionParser =
  let move = do
        pos1 <- positionParser
        pos2 <- char '-' *> positionParser
        return Action { positions = [pos1, pos2] }
      attack = do
        pos1  <- positionParser
        other <- many1 $ char 'x' *> positionParser
        return $ Action { positions = pos1 : other }
  in  try attack <|> move

letterToInt :: Char -> Int
letterToInt l | l == 'a' = 1
              | l == 'b' = 2
              | l == 'c' = 3
              | l == 'd' = 4
              | l == 'e' = 5
              | l == 'f' = 6
              | l == 'g' = 7
              | l == 'h' = 8

positionParser :: Parser Position
positionParser = do
  letter <- oneOf "abcdefgh"
  number <- (fmap digitToInt (oneOf "12345678"))
  return Position { position = (letterToInt letter, number) }

resultParser :: Parser Result
resultParser = try winnerParser <|> drawParser <|> infoParser

winnerParser :: Parser Result
winnerParser = Winner <$> (try winnerWhiteParser <|> winnerBlackParser)

winnerWhiteParser :: Parser Side
winnerWhiteParser = White <$ ((char '2' <|> char '1') <* string "-0")


winnerBlackParser :: Parser Side
winnerBlackParser = Black <$ (string "0-" <* (char '2' <|> char '1'))

drawParser :: Parser Result
drawParser = Draw <$ (string "1-1" <|> string "1/2-1/2")

infoParser :: Parser Result
infoParser = NoInfo <$ (string "*")
