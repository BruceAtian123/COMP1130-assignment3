module State where

import Board
import Player

data State = State
  { status :: GameStatus
  , board :: Board
  , turn :: Player
  , movesLeft :: [Int]
  , wPips :: Int
  , bPips :: Int
  , wScore :: Int
  , bScore :: Int
  } deriving (Show)

data GameStatus
  = Playing
  | Finished
  deriving (Eq, Show)

type Moves = [Move]

type Lookahead = Int

-- Move = (p, n) means move piece at point p forward n points
type Move = (Int, Int)

initialState :: Player -> State
initialState p = updatePips $ State Playing b p [] 0 0 0 0
  where
    b =
      case p of
        White -> initialBoard
        Black -> reverseBoard initialBoard

swapTurn :: State -> State
swapTurn s = s {turn = otherPlayer (turn s)}

stateAfterDiceRoll :: State -> (Int, Int) -> State
stateAfterDiceRoll s roll = s {movesLeft = newMoves roll}
  where
    newMoves :: (Int, Int) -> [Int]
    newMoves (a, b)
      | a == b = replicate 4 a
      | otherwise = [a, b]

updatePips :: State -> State
updatePips s =
  case turn s of
    White ->
      s
        { wPips = wBP + sum (map fst totalPips)
        , bPips = bBP + sum (map snd totalPips)
        }
    Black ->
      s
        { bPips = bBP + sum (map fst totalPips)
        , wPips = wBP + sum (map snd totalPips)
        }
  where
    b = board s
    wBP = 25 * wBar b
    bBP = 25 * bBar b
    totalPips = zipWith pointToPipTuple (points b) [1 .. (length (points b))]
        -- Returns (currentPlayer, oppositePlayer)
    pointToPipTuple :: Point -> Int -> (Int, Int)
    pointToPipTuple p i =
      case p of
        Nothing -> (0, 0)
        Just (player, amount)
          | player == turn s -> (amount * i, 0)
          | otherwise -> (0, amount * (25 - i))
