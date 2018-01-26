{-# LANGUAGE LambdaCase #-}
module Dalmuti.Rules where
import Dalmuti.Types
import Data.List
import Data.List.Split

amountInDeck :: Card -> Int
amountInDeck Joker = 2
amountInDeck card = fromEnum card + 1

standardDeck :: [Card]
standardDeck = concatMap (\card -> replicate (amountInDeck card) card) [One ..]

canPlayGroupOn :: Group -> Pile -> Bool
canPlayGroupOn (Group card amount) Nothing = True
canPlayGroupOn (Group card amount) (Just (Group currentCard currentAmount)) = card < currentCard && amount == currentAmount

playableGroups :: Hand -> Pile -> [Group]
playableGroups hand pile = filter (`canPlayGroupOn` pile) $ groups hand

createGroup :: [Card] -> Group
createGroup cards
  | all (== head cards) cards = Group (head cards) (length cards)
  | otherwise = error $ "Could not create group from " ++ show cards

groupToCards :: Group -> [Card]
groupToCards (Group card amount) = replicate amount card

groups :: Hand -> [Group]
groups hand = g where
  (jokers, noJokers) = partition (== Joker) hand
  groupsWithoutJoker = groupsNoJoker noJokers
  g = addJokersToGroups (length jokers) groupsWithoutJoker

addJokersToGroups :: Int -> [Group] -> [Group]
addJokersToGroups 0 group = group
addJokersToGroups amount group = addJokersToGroups (amount - 1) (group ++ map (\case Group card amount -> Group card (amount + 1)) group)

groupsNoJoker :: Hand -> [Group]
groupsNoJoker hand = nub
  $ map createGroup
  $ filter (not . null)
  $ concatMap subsequences (group $ sort hand)

deal :: Int -> Deck -> [Hand]
deal players deck = transpose $ chunksOf players deck

makePlayers :: Int -> Deck -> [Player]
makePlayers amount deck = map (\hand -> Player hand Pending) (deal amount deck)

playGroup :: Group -> GameState -> GameState
playGroup group (GameState hands _) = GameState hands (Just group)

hand :: Player -> Hand
hand (Player hand _) = hand

testGame = GameState (makePlayers 5 standardDeck) Nothing