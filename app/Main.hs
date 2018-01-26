module Main where

import Dalmuti.Rules
import Dalmuti.Types
import System.Random.Shuffle
import Data.List

main :: IO ()
main = do
  putStrLn "-- The Great Dalmuti --"
  putStrLn "How many players are there?"
  playerInput <- getLine

  shuffledDeck <- shuffleM standardDeck

  let players = makePlayers (read playerInput :: Int) shuffledDeck
  let state = GameState players Nothing
  playHumanTurn (hand $ head players) Nothing

  pure ()


playAiTurn :: Hand -> Pile -> Group
playAiTurn hand pile = last $ playableGroups hand pile

playHumanTurn :: Hand -> Pile -> IO Group
playHumanTurn hand pile = do
  putStrLn $ "Your hand is " ++ show hand
  putStrLn "Select [number]:"
  let groups = playableGroups hand pile
  let groupString = zipWith (\index group -> "[" ++ show index ++ "]: " ++ show group) [0..] groups
  putStrLn $ intercalate "\n" groupString
  chosenGroup <- (\index -> groups !! index) <$> (\input -> read input :: Int) <$> getLine
  pure chosenGroup