module Player where

import Test.QuickCheck (Arbitrary, arbitrary, oneof)

data Player
  = White
  | Black
  deriving (Show, Eq)

instance Arbitrary Player where
  arbitrary = oneof $ fmap return [Black, White]

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White