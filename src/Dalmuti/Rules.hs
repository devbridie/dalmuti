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
playableGroups hand pile = filter (`canPlayGroupOn` pile) $ largestGroups hand

createGroup :: [Card] -> Group
createGroup cards = Group (head cards) (length cards) -- TODO: needs some verification

groupToCards :: Group -> [Card]
groupToCards (Group card amount) = replicate amount card

largestGroups :: Hand -> [Group]
largestGroups hand = map createGroup $ group $ sort hand

groups :: Hand -> [Group]
groups hand = nub
  $ map createGroup
  $ filter (not . null)
  $ concatMap (subsequences . groupToCards) (largestGroups hand)

deal :: Int -> Deck -> [Hand]
deal players deck = transpose $ chunksOf players deck

makePlayers :: Int -> Deck -> [Player]
makePlayers amount deck = map (\hand -> Player hand Pending) (deal amount deck)

playGroup :: Group -> GameState -> GameState
playGroup group (GameState hands _) = GameState hands (Just group)

testGame = GameState (makePlayers 5 standardDeck) Nothing