module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Safe (readMay)
import System.Mem (performMajorGC)
import System.Random (randomRIO)
import qualified System.Timeout (timeout)
import Text.Printf (printf)

import Move
import Options
import Player (Player(Black, White))
import State

import qualified PlayAsBlack (makeMove)
import qualified PlayAsWhite (makeMove)

type Dice = (Int, Int)

main :: IO ()
main = argumentHandler mainWithOpts

mainWithOpts :: Options -> IO ()
mainWithOpts = flip newGame (0, 0)

newGame :: Options -> (Int, Int) -> IO ()
newGame opts (bs, ws)
  | bs >= playTo opts =
    putStrLn $ "Black player wins " ++ show bs ++ " to " ++ show ws ++ "!"
  | ws >= playTo opts =
    putStrLn $ "White player wins " ++ show ws ++ " to " ++ show bs ++ "!"
  | otherwise = do
    putStrLn "New game..."
    putStrLn "Current match stats: "
    putStrLn $ "    Black (X): " ++ show bs
    putStrLn $ "    White (O): " ++ show ws
    putStrLn $ "Playing to: " ++ show (playTo opts)
    putStrLn $ "Time out is: " ++ show (timeout opts)
    putStrLn "----------\nNew game started:"
    dice <- getDice True
    putStrLn $ "Black rolls a " ++ show (fst dice)
    putStrLn $ "White rolls a " ++ show (snd dice)
    let firstP = whichPlayerGoesFirst dice
    putStrLn $ show firstP ++ " goes first!"
    let state = (initialState firstP) {wScore = ws, bScore = bs}
    runMatch (Just dice) state
  where
    runMatch :: Maybe Dice -> State -> IO ()
    runMatch dM s =
      print s >>
      case (status s, dM) of
        (Finished, _) -> newGame opts (bScore s, wScore s)
        (_, Nothing) -> getDice False >>= flip playMatch s
        (_, Just dice) -> playMatch dice s
    playMatch :: Dice -> State -> IO ()
    playMatch dice s
      | human opts && turn s == Black = do
        showDice Black dice
        hMoves <- getMovesFromHuman [] diceRollState
        runMatch Nothing $ performMoves diceRollState $ reverse hMoves
      | otherwise = do
        showDice player dice
        putStrLn $ show player ++ " is making a move..."
        moves <- getMovesFromAI diceRollState opts
        runMatch Nothing $ performMoves diceRollState moves
      where
        diceRollState :: State
        diceRollState = stateAfterDiceRoll s dice
        player :: Player
        player = turn s

getMovesFromAI :: State -> Options -> IO Moves
getMovesFromAI s opts = do
  maybeMoves <- tryToGetMoves Nothing 1 0 0
  case maybeMoves of
    Nothing -> error $ show (turn s) ++ " failed to return moves in time"
    Just moves -> return moves
  where
    globalTimeout :: Int
    globalTimeout = timeout opts * 1000000
    stallDetection :: Double
    stallDetection = 1.1
    noOfStallsTolerated :: Int
    noOfStallsTolerated = 2
    tryToGetMoves ::
         Maybe Moves -> Lookahead -> Double -> Int -> IO (Maybe Moves)
    tryToGetMoves mM look prevTime stalls = do
      performMajorGC
      startTime <- getCurrentTime
      moves <- System.Timeout.timeout globalTimeout $ return $! makeMove s look
      endTime <- getCurrentTime
      let timeUsed = (realToFrac $ diffUTCTime endTime startTime) :: Double
      putStrLn $ showThreeDigits (timeUsed * 1000) ++ " ms"
      case moves of
        Nothing -> return mM -- timeUsed > globalTimeout
        _ ->
          case timeUsed > prevTime * stallDetection of
            True -> tryToGetMoves moves (look + 1) timeUsed 0
            _
              | stalls == noOfStallsTolerated -> return mM
              | otherwise ->
                tryToGetMoves moves (look + 1) timeUsed (stalls + 1)
    makeMove :: (State -> Lookahead -> Moves)
    makeMove =
      case turn s of
        Black -> PlayAsBlack.makeMove
        White -> PlayAsWhite.makeMove
    showThreeDigits :: Double -> String
    showThreeDigits = printf "%1.3f"

getMovesFromHuman :: Moves -> State -> IO Moves
getMovesFromHuman ms s =
  case legalMoves s of
    [] -> return ms
    legal -> do
      print s
      putStrLn $ "Available moves are: " ++ show legal
      putStrLn "Enter one of them:"
      input <- getLine
      if input == ""
        then do
          let move = head legal
          putStrLn $ "No input. Here's the move: " ++ show move
          getMovesFromHuman (move : ms) $ performSingleMove s move
        else case readMay input of
               Nothing -> putStrLn "Input error" >> getMovesFromHuman ms s
               Just move
                 | move `elem` legal -> do
                   let newState = performSingleMove s move
                   putStrLn "Accepted"
                   getMovesFromHuman (move : ms) newState
                 | otherwise ->
                   putStrLn "Not a legal move" >> getMovesFromHuman ms s

showDice :: Player -> Dice -> IO ()
showDice p (d1, d2) = putStrLn $ show p ++ "'s roll is: " ++ show d1 ++ "-" ++ show d2

whichPlayerGoesFirst :: Dice -> Player
whichPlayerGoesFirst (b, w)
  | b > w = Black
  | otherwise = White

getDice :: Bool -> IO Dice
getDice noDup = do
  r1 <- randomRIO (1, 6)
  r2 <- randomRIO (1, 6)
  case noDup of
    True
      | r1 == r2 -> getDice noDup
      | otherwise -> return (r1, r2)
    _ -> return (r1, r2)
