{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
#-}

{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Hanabi where
import System.Environment
import System.Directory
import Control.Monad
import System.IO  
import Data.List
import qualified Data.MultiSet as MSet
import Utilities

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x
--same as $
--(b -> c) -> (a -> b) -> a -> c

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

type Color = Int
type PlayerNum = Int

data CardType = Number Int | Suit Color

data Card = Card { _number :: Int, _suit :: Color } deriving (Eq, Ord)
data Hint = Hint { _playerNum :: PlayerNum, _cardtype :: CardType}

ofClass :: CardType -> Card -> Bool
ofClass cl c = 
  case cl of 
    Number k -> _number c == k
    Suit k -> _suit c == k

data Move = Discard Int | Play Int | GiveHint Hint

getHint :: Hint -> State -> [Int]
getHint h st = 
  let
    hand = (_hands st)!!(_playerNum h)
  in
    findIndices (ofClass (_cardtype h)) hand

data State = State { _hands :: [[Card]], _deck :: [Card], _played :: [Int], _hints :: Int, _lives :: Int, _cardsLeft :: Int, _numPlayers :: Int, _turnNum :: Int, _currentPlayer :: Int, _discardPile :: MSet.MultiSet Card}

--http://brandon.si/code/a-brief-tutorial-introduction-to-fclabels-1-0/
$(mkLabels [''Card, ''Hint, ''State])

--remove info about kth player and deck to give to kth player
scrub :: Int -> State -> State 
scrub k st = 
  let 
    --remove info about own hand
    hands2 = listUpdate k [] (_hands st)
    --remove info on deck
  in
    st {_hands = hands2, _deck = []}

class Player a where
--(player who made move, move), [cards hinted], state of game, a
--update player's knowledge
  num :: a -> Int
  update :: (Int, Move) -> [Int] -> State -> a -> a
  getMove :: State -> a -> (Move, a)

dealHands' ::[[Card]] -> [Card] -> Int -> ([Card], [[Card]])
dealHands' hs d k = 
  if k == 0 then (d, hs) else dealHands' ((take 5 d):hs) (drop 5 d) (k-1)

dealHands = dealHands' [[]]

startGame :: Int -> [Card] -> State
startGame ps cards = 
  let 
    (d, hs) = dealHands cards ps
  in
    State {_hands = hs , _deck = d, _played = replicate ps 0, _hints = 8, _lives = 3, _cardsLeft = length d, _numPlayers = ps, _turnNum = 0, _currentPlayer = 0, _discardPile = MSet.empty}

getCard :: Int -> PlayerNum -> State -> Card
getCard i pn st = ((_hands st)!!pn)!!i

tryPlay :: Card -> State -> State
tryPlay card st = 
  let
    suit1 = _suit card
    playedUpTo = (_played st)!!suit1
  in
    st |> doIfElse 
--if
            (suit1 == playedUpTo + 1) 
--then
            ((modify played) (listUpdateFun suit1 (+1)))
--else
            ((modify lives) (\x -> x - 1))

turn :: (Player a) => ([a], State) -> ([a], State)
turn (ps,st) =
  let 
    n = _currentPlayer st
    curPlayer = ps!!n
    (curMove, curPlayer2) = getMove (scrub n st) curPlayer
--try to draw a card
    ps2 = listUpdate n curPlayer2 ps
    topCard = (_deck st)!!0
--    topCard = if _deck st == [] then Nothing else Just (deck!!0)
--effects of move
    newSt = 
      (case curMove of
        Discard k ->  
          --modify the hand by updating the kth card with the top card from the deck --actually not what we want.
          st |> (modify hands) (listUpdate n (listUpdate k topCard ((_hands st)!!n)))
          --delete top card from deck
             |> (modify deck) (drop 1)
          --add card to discard
             |> (modify discardPile) (MSet.insert (getCard k n st))
        Play k -> 
          st |> (modify hands) (listUpdate n (listUpdate k topCard ((_hands st)!!n)))
             |> (modify deck) (drop 1)
             |> (tryPlay (getCard k n st))
        GiveHint _ -> 
          st |> (modify hints) (\x -> x - 1)) |> (modify turnNum (+1))
--assume hint is valid for now. NEED TO CHECK!
--also subtlety: hint can't be empty!
    hintCards = 
      case curMove of
        GiveHint h -> getHint h st
        _ -> []
--update players
    newPlayers = map (update (n,curMove) hintCards newSt) ps2
-- (Int, Move) -> [Int] -> State -> a -> a
  in 
    (newPlayers, newSt)

playHanabi :: (Player a) => [a] -> [Card] -> ([a], State)
playHanabi players cards = 
  let
    startState = startGame (length players) cards
  in
    loopUntil (\(ps,st) -> _lives st <= 0 || length (_deck st) <= 0) turn (players, startState)
--actually, we want to allow 1 more turn... 
