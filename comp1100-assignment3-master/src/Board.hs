-- Copyright 2018 The Australian National University, All rights reserved
module Board where

import Player

import Data.List (intercalate)
import Test.QuickCheck

type Point = Maybe (Player, Int)

data Board = Board
  { points :: [Point]
  , wBar :: Int
  , bBar :: Int
  }

instance Arbitrary Board where
  arbitrary = do
    list <- suchThat arbitrary (\x -> length x == 24)
    let numList = take 24 $ map (\x -> abs x `mod` 8) list :: [Int]
    player <- suchThat arbitrary (\x -> length x == 24)
    let pointList = zipWith genPoint numList player
    return $ Board pointList 0 0
    where
      genPoint :: Int -> Player -> Point
      genPoint i p
        | i == 0 = Nothing
        | otherwise = Just (p, i)

instance Show Board where
  show b =
    "\n  " ++
    intercalate "  " [show x | x <- [13 .. 18 :: Int]] ++
    "    " ++
    intercalate "  " [show x | x <- [19 .. 24 :: Int]] ++
    "\n" ++
    replicate 51 '-' ++
    "\n" ++
    intercalate "\n" (map (formatLine . produceLine) [1 .. 6]) ++
    "\n" ++
    replicate 51 '-' ++
    barLine ++
    "\n" ++
    replicate 51 '-' ++
    "\n" ++
    intercalate "\n" (reverse $ map (formatLine . produceLine) [7 .. 12]) ++
    "\n" ++
    replicate 51 '-' ++
    "\n" ++
    "  " ++
    intercalate "  " [show x | x <- [12, 11, 10, 9 :: Int]] ++
    "   " ++
    intercalate "   " [show x | x <- [8, 7 :: Int]] ++
    "     " ++ intercalate "   " [show x | x <- [6,5 .. 1 :: Int]] ++ "\n"
    where
      produceLine :: Int -> String
      produceLine i
                -- Top Heavy
        | i <= 6 = concatMap (\x -> pointToString x i True) topHalf
        | otherwise =
          concatMap (\x -> pointToString x (i - 6) False) $ reverse bottomHalf
      topHalf, bottomHalf :: [Point]
      topHalf = drop 12 $ points b
      bottomHalf = take 12 $ points b
      formatLine :: String -> String
      formatLine str =
        "| " ++
        intercalate " | " [[c] | c <- take 6 str] ++
        " ||| " ++ intercalate " | " [[c] | c <- drop 6 str] ++ " |"
      barLine :: String
      barLine
        | totalBar == 0 = []
        | otherwise =
          "\n" ++
          replicate (26 - totalBar) ' ' ++
          unwords [[c] | c <- barString]
        where
          totalBar :: Int
          totalBar = wBar b + bBar b
          barString :: String
          barString = replicate (wBar b) 'o' ++ replicate (bBar b) 'x'
      pointToString :: Point -> Int -> Bool -> String
      pointToString p i t =
        case p of
          Nothing -> " "
          Just (pl, n)
            | n > 9 && i == 5 && t -> take 1 $ show n
            | n > 9 && i == 6 && t -> drop 1 $ show n
            | n > 9 && i == 5 -> drop 1 $ show n
            | n > 9 && i == 6 -> take 1 $ show n
            | n > 6 && i == 6 -> show n
            | n >= i ->
              if pl == White
                then "o"
                else "x"
            | otherwise -> " "

initialBoard :: Board
initialBoard =
  Board
    [ Just (Black, 2)
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just (White, 5)
    , Nothing
    , Just (White, 3)
    , Nothing
    , Nothing
    , Nothing
    , Just (Black, 5)
    , Just (White, 5)
    , Nothing
    , Nothing
    , Nothing
    , Just (Black, 3)
    , Nothing
    , Just (Black, 5)
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Just (White, 2)
    ]
    0
    0

reverseBoard :: Board -> Board
reverseBoard b = b {points = reverse $ points b}

updatePointOnBoard :: Board -> Int -> Point -> Board
updatePointOnBoard b i' p =
  b {points = take i (points b) ++ [p] ++ drop (i + 1) (points b)}
  where
    i = i' - 1

placeOneMoreOnBar :: Player -> Board -> Board
placeOneMoreOnBar p b =
  case p of
    White -> b {wBar = wBar b + 1}
    Black -> b {bBar = bBar b + 1}

removeOneFromBar :: Player -> Board -> Board
removeOneFromBar p b =
  case p of
    White -> b {wBar = wBar b - 1}
    Black -> b {bBar = bBar b - 1}

moveForward :: Board -> Player -> Int -> Int -> Board
moveForward b p f n =
  moveForward' {points = map correctPointNumbering (points moveForward')}
  where
    correctPointNumbering :: Point -> Point
    correctPointNumbering point =
      case point of
        Nothing -> point
        Just (_, k)
          | k == 0 -> Nothing
          | otherwise -> point
    moveForward' :: Board
    moveForward'
      | f == 0 =
        case points b !! (24 - n) of
          Nothing -> removeOneFromBar p $ updatePointFromBar $ Just (p, 1)
          Just (occupier, k)
            | occupier == p ->
              removeOneFromBar p $ updatePointFromBar $ Just (p, k + 1)
            | k == 1 ->
              placeOneMoreOnBar occupier $
              removeOneFromBar p $ updatePointFromBar $ Just (p, 1)
            | otherwise -> b
      | f == n =
        case fromPoint of
          Just (_, k) -> updatePointOnBoard b f $ Just (p, k - 1)
          _ -> b
      | otherwise =
        case (fromPoint, toPoint) of
          (Nothing, _) -> b
          (Just (p1, k1), Nothing) ->
            updatePoints b (Just (p1, k1 - 1)) $ Just (p, 1)
          (Just (p1, k1), Just (p2, k2))
            | p1 == p2 -> updatePoints b (Just (p1, k1 - 1)) $ Just (p2, k2 + 1)
            | k2 == 1 ->
              placeOneMoreOnBar p2 $
              updatePoints b (Just (p1, k1 - 1)) $ Just (p1, 1)
            | otherwise -> b
    fromPoint = points b !! (f - 1)
    toPoint = points b !! (toIndex - 1)
    toIndex = f - n
    updatePointFromBar :: Point -> Board
    updatePointFromBar = updatePointOnBoard b (25 - n)
    updatePoints :: Board -> Point -> Point -> Board
    updatePoints =
      flip flip toIndex . (updatePointOnBoard .) . flip updatePointOnBoard f
