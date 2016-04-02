{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Random.Tabletop
  ( parseRolls
  , rollGeneric
  , RollResult
  , SuccessMessage
  , FailureMessage
  ) where

import Control.Monad
import Control.Monad.Except
import Data.Maybe(fromMaybe,fromJust,isNothing,isJust)
import Control.Monad.Trans.Class
import System.Process(readProcessWithExitCode)
import Control.Monad.IO.Class(liftIO)
import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import Text.Megaparsec
import Text.Megaparsec.Lexer hiding (space)
import Data.Text hiding (take)
import Control.Monad.Random

--Common things
type FailureMessage = Text
type SuccessMessage = (Text, Text) --Roll message, result message
type RollParseResult = Either FailureMessage SuccessMessage

showT :: (Show s) => s -> Text
showT = pack . show

parseRollWithGen :: forall g. (RandomGen g) => Text -> g -> Either ParseError RollParseResult
parseRollWithGen msg gen = 
  case parse allRollsParser "" msg of
    Left err -> Left err
    Right (rollM :: Rand g RollParseResult) -> Right $ evalRand rollM gen

allRollsParser = do
  cmd <- someTill anyChar (space <|> eof)
  manyTill anyChar (space <|> eof)
  rest <- pack <$> manyTill anyChar eof
  case cmd of
    "roll" -> return $ rollGeneric rest
    _ -> fail "No such command."


--Specific things
data GenericRoll = GenericRoll Integer Integer --numDice numSides

rollGeneric :: (RandomGen g) => Text -> Rand g RollParseResult
rollGeneric msg = 
  case parse genericParser "" msg of
    Left err -> return . Left $ showT err
    Right (GenericRoll numDice numSides) -> do
      rolls <- take (fromIntegral numDice) <$> getRandomRs (1, numSides)
      return . Right $ (msg, showT rolls)

genericParser = do
  numDice <- integer
  char 'd'
  numSides <- integer
  eof
  return $ GenericRoll numDice numSides

