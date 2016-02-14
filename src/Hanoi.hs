module Hanoi (hanoi) where

import Data.List

type Peg = String
type Move = (Peg, Peg)
type PegsDiscs = [Integer]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDiscs a b c =
  let aDiscs = [1..numDiscs]
      bDiscs = []
      cDiscs = []
  in
    []

rots xs = init (zipWith (++) (tails xs) (inits xs))

-- Data.List permutations:
-- permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
mapOverMoves = map (tryPeg1toOtherPegs "A" "B" "C" [1,2,3] [] []) []

tryPeg1toOtherPegs :: Peg -> Peg -> Peg -> PegsDiscs -> PegsDiscs -> PegsDiscs -> [Maybe Move] -> [[Maybe Move]]
tryPeg1toOtherPegs peg1 peg2 peg3  peg1Discs peg2Discs peg3Discs movesTilNow@(Nothing:_) = [movesTilNow]
tryPeg1toOtherPegs peg1 peg2 peg3 peg1Discs peg2Discs peg3Discs movesTilNow =
  let
    peg1ToPeg2Moves  = if tryingToReverseMove peg1 peg2 movesTilNow then
                        movesTilNow
                       else
                        tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
    pet1ToPeg3Moves  = if tryingToReverseMove peg1 peg3 movesTilNow then
                        movesTilNow
                       else
                        tryPeg1toPeg2 peg1 peg3 peg1Discs peg3Discs movesTilNow
  in
    [peg1ToPeg2Moves, pet1ToPeg3Moves]


tryPeg1toPeg2 :: Peg -> Peg -> PegsDiscs -> PegsDiscs -> [Maybe Move] -> [Maybe Move]
tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
  | null peg1Discs = Nothing:movesTilNow
  | null peg2Discs = Just (peg1,peg2):movesTilNow
  | head peg1Discs < head peg2Discs = Just (peg1,peg2):movesTilNow
  | otherwise = Nothing:movesTilNow

tryingToReverseMove :: Peg -> Peg -> [Maybe Move] -> Bool
tryingToReverseMove peg1 peg2 [] = False
tryingToReverseMove peg1 peg2 (headMove:_) = headMove == Just (peg2, peg1)
