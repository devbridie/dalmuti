module Dalmuti.Types where

data Card = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven | Twelve | Joker
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Deck = [Card]
type Hand = [Card]
type Pile = Maybe Group
data Group = Group Card Int
  deriving (Eq)
instance Show Group where
  show (Group card amount) = show amount ++ "x" ++ (show card)

data GameState = GameState [Player] Pile
  deriving Show

data Player = Player Hand PlayerState
  deriving Show

data PlayerState = Passed | Pending
  deriving Show