module Move where

import Board
import Player
import State

import Data.List (maximumBy, delete)
import Data.Maybe (isNothing, isJust)
import Data.Ord (comparing)
import Data.Tree (Tree, unfoldTree)

performMoves :: State -> [Move] -> State
performMoves s m
  | status s == Finished = s
  | otherwise =
    let newState = foldl performSingleMove s m
     in case legalMoves newState of
          []
            | gameHasFinished newState -> scoreUpdateAtFinish newState
            | otherwise -> endMoveStateUpdate newState
          _ -> error "Did not make enough moves."
  where
    endMoveStateUpdate :: State -> State
    endMoveStateUpdate s' =
      s'
        { board = reverseBoard $ board s'
        , turn = otherPlayer $ turn s
        , movesLeft = []
        }
    gameHasFinished :: State -> Bool
    gameHasFinished s' = not (any (isOwnPoint' s') (points $ board s'))

scoreUpdateAtFinish :: State -> State
scoreUpdateAtFinish s =
  case turn s of
    White -> s {status = Finished, wScore = wScore s + 1, movesLeft = []}
    Black -> s {status = Finished, bScore = bScore s + 1, movesLeft = []}

performSingleMove :: State -> Move -> State
performSingleMove s m@(i, n)
  | not $ isLegalMove s m = s
  | n `elem` movesLeft s = s' {movesLeft = delete n (movesLeft s)}
  | otherwise = s' {movesLeft = drop 1 (movesLeft s)}
  where
    s' :: State
    s' = updatePips $ s {board = moveForward (board s) (turn s) i n}

legalMoves :: State -> Moves
legalMoves s = [(x, y) | x <- [0 .. 24], y <- [1 .. 6], isLegalMove s (x, y)]

isLegalMove :: State -> Move -> Bool
isLegalMove s (i, n)
  | null $ movesLeft s = False
  | not validNumber || not (isOwnPoint s i) = False
  | i == 0 = onTheBar && okayToMoveTo (25 - n)
  | allInHomeBoard =
    case n `elem` movesLeft s of
      True
        | n == i -> isJust (point s i)
        | n < i -> isJust (point s i) && okayToMoveTo (i - n)
        | otherwise -> False
      False -> i == highestHomePoint && i == n && n < maximum (movesLeft s)
  | otherwise =
    not onTheBar && isJust (point s i) && n < i && okayToMoveTo (i - n)
  where
    validNumber :: Bool
    validNumber = n >= 1 && n <= 6 && (allInHomeBoard || n `elem` movesLeft s)
    onTheBar :: Bool
    onTheBar =
      case turn s of
        White -> wBar (board s) > 0
        Black -> bBar (board s) > 0
    okayToMoveTo :: Int -> Bool
    okayToMoveTo i' =
      maybe True (\(p, n') -> p == turn s || n' == 1) $ point s i'
    allInHomeBoard :: Bool
    allInHomeBoard =
      all isNothing $ filter (isOwnPoint' s) $ drop 6 $ points $ board s
    highestHomePoint :: Int
    highestHomePoint =
      fst $
      maximumBy (comparing fst) $
      filter (isOwnPoint s . fst) $ zip [1 .. 24] (points $ board s)

legalMovesTree :: State -> Tree State
legalMovesTree =
  unfoldTree f
    where
      f :: State -> (State, [State])
      f s = (s, map (performSingleMove s) (legalMoves s))

point :: State -> Int -> Point
point s n = points (board s) !! (n - 1)

isOwnPoint :: State -> Int -> Bool
isOwnPoint s x = x == 0 || isOwnPoint' s (point s x)

isOwnPoint' :: State -> Point -> Bool
isOwnPoint' s = maybe False (\(a, _) -> a == turn s)
