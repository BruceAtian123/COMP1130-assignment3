module PlayAsWhite where

import State
import Move
import Board

type Dice = (Int, Int)

type Forest a = [BTree a]

data BTree a = BNode a [BTree a]
    deriving (Show, Eq)

fststate :: State -> Dice -> State
fststate s (d1,d2) = stateAfterDiceRoll s (d1,d2)

getallstates :: State -> [State]
getallstates s = performSingleMove s <$> legalMoves s

getallmovesandstates :: State -> [(State,Move)]
getallmovesandstates s = zip (getallstates s) (legalMoves s)

buildTree :: (State -> [State]) -> State -> BTree State
buildTree getallstates' s = BNode s (buildTree getallstates' <$> getallstates' s)

prune :: Lookahead -> BTree a -> BTree a
prune _ (BNode a []) = BNode a []
prune 0 (BNode a _) = BNode a []
prune n (BNode a xs) = BNode a (map (prune (n - 1)) xs)

eval :: State -> Int
eval s
    | bPips s < wPips s = bPips s - wPips s
    | otherwise = (bPips s - wPips s) + (bBar (board s) - wBar (board s))*20

mapTree :: (a -> b) -> BTree a -> BTree b
mapTree q = foldTree (BNode . q) (:) []
    where foldTree q' g a (BNode label subtrees) = q' label (foldr (g . foldTree q' g a) a subtrees)

maximise :: Ord a => BTree a -> a
maximise (BNode n []) = n
maximise (BNode _ subtrees) = maximum (map minimise subtrees)

minimise :: Ord a => BTree a -> a
minimise (BNode n []) = n
minimise (BNode _ subtrees) = minimum (map maximise subtrees)

minimax :: Lookahead -> State -> Int
minimax l = maximise . mapTree eval . prune l . unfoldTree w'
      where
      unfoldTree :: (b -> (a, [b])) -> b -> BTree a
      unfoldTree w b = let (a, bs) = w b in BNode a (unfoldForest w bs)
      unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
      unfoldForest wb = map (unfoldTree wb)
      w' :: State -> (State, [State])
      w' s = (s, map (performSingleMove s) (legalMoves s))

getmaxinmin :: [(State,Move)] -> (State,Move)
getmaxinmin ms = case ms of
    [(s1,m1)]   ->  (s1,m1)
    (s1,m1):(s2,m2):l
        | eval s1 >= eval s2 -> getmaxinmin ((s1,m1):l)
        | eval s1 < eval s2 -> getmaxinmin ((s2,m2):l)
    _ -> undefined

getmininmax :: [(State,Move)] -> (State,Move)
getmininmax ms = case ms of
    [(s1,m1)]   -> (s1,m1)
    (s1,m1):(s2,m2):l
        | eval s1 <= eval s2 -> getmininmax ((s1,m1):l)
        | eval s1 >  eval s2 -> getmininmax ((s2,m2):l)
    _ -> undefined

minimax' :: State -> [(State,Move)]
minimax' s
    | null(legalMoves s) = []
    | otherwise = getmaxinmin (getallmovesandstates s) : minimax' (fst (getmaxinmin(getallmovesandstates s)))

makeMove :: State -> Lookahead -> Moves
makeMove s _ = snd <$> minimax' s

{-alphabeta :: State -> GameStatus -> Lookahead -> Int
  alphabeta _ status l
      | l == 0 || status == Finished = eval board
      | otherwise = minormax

  minormax :: State -> Player -> Int
  minormax _ turn
      | turn == White = maximiserplayer
      | turn == Black = minimiserplayer

  maximiserplayer :: State -> Double -> Lookahead
  maximiserplayer _ (a,b) = snd $ foldr (\s' (a',v) ->
                          let newV = max v (minimiserplayer s' (a',b)) in
                          if v >= b
                          then (a',v)
                          else (max a' newV, newV))

  minimiserplayer :: State -> Double -> Lookahead
  minimiserplayer _ (a,b) = snd $ foldr (\s' (b',v) ->
                          let newV = max v (maximiserplayer s' (a,b')) in
                          if v <= a
                          then (b',v)
                          else (min b' newV, newV))
-}