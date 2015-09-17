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
import Control.Lens
import Prelude hiding ((.), id)

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

data State = State { _hands :: [[Card]], _deck :: [Card], _played :: [Int], _hints :: Int, _lives :: Int, _cardsLeft :: Int, _numPlayers :: Int, _turnNum :: Int, _currentPlayer :: Int, _discardPile :: MSet.MultiSet Card, _countdown :: Int}

makeLenses ''Card
makeLenses ''Hint
makeLenses ''State

--remove info about kth player and deck to give to kth player
scrub :: Int -> State -> State 
scrub k st =
  st & (hands . ix k) .~ []
     & (deck) .~ []

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
    State {_hands = hs , _deck = d, _played = replicate ps 0, _hints = 8, _lives = 3, _cardsLeft = length d, _numPlayers = ps, _turnNum = 0, _currentPlayer = 0, _discardPile = MSet.empty, _countdown = ps}

getCard :: Int -> PlayerNum -> State -> Card
getCard i pn st = ((_hands st)!!pn)!!i

restoreHint :: State -> State
restoreHint = hints %~ (doIfCond (<8) (+1))

tryPlay :: Card -> State -> State
tryPlay card st = 
  let
    suit1 = _suit card
    playedUpTo = (_played st)!!suit1
  in
    st & doIfElse 
--if
            (suit1 == playedUpTo + 1) 
--then
            --if finish a color, then restore 1 hint.
            ((played . ix suit1) +~ 1 |>> doIf (_number card == 5) restoreHint)
--else
            (lives -~ 1)


--should be Except 
turn :: (Player a) => ([a], State) -> ([a], State)
turn (ps,st) =
  let 
    n = _currentPlayer st
    curPlayer = ps!!n
    (curMove, curPlayer2) = getMove (scrub n st) curPlayer
--try to draw a card
    ps2 = ps & ix n .~ curPlayer2
--    topCard = st & (deck . ix 0) ^? 0
    topCard = (_deck st) `mindex` 0
--effects of move
    removeAndDraw k s = s & (hands . ix n) %~ replaceSublist k (k+1) []
                          & (case topCard of
                              Just topC ->
                                (hands . ix n) %~ (topC:) |>>
                                deck %~ (drop 1)
                              Nothing -> id)    
    newSt = st & (case topCard of
                   Nothing -> countdown -~ 1
                   _ -> id)
               & (case curMove of
                   Discard k ->           
                     removeAndDraw k
          --add card to discard
                      |>> (discardPile) %~ (MSet.insert (getCard k n st))
                      |>> restoreHint
                   Play k -> 
                     removeAndDraw k
                      |>> (tryPlay (getCard k n st))
                   GiveHint _ -> 
                     (hints) -~ 1
                      |>> (turnNum +~ 1))
--assume hint valid (nonempty) NEED TO FIX with error handling.
    hintCards = 
      case curMove of
        GiveHint h -> getHint h st
        _ -> []
        {-
    curMove' = case curMove of GiveHint _ -> case hintCards of [] -> InvalidMove
                                                               _ -> curMove
                               _ -> curMove-}
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
    loopUntil (\(ps,st) -> _lives st <= 0 || _countdown st <= 0) turn (players, startState)
--actually, we want to allow 1 more turn... 
