module Hanoi (hanoi) where

import Data.List

type Peg = String
type Move = (Peg, Peg)
type PegDiscs = [Integer]
type PegDiscState = (PegDiscs, PegDiscs, PegDiscs)

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
--mapOverMoves = map (tryPeg1toOtherPegs "A" "B" "C") ()

tryPeg1toOtherPegs :: Peg -> Peg -> Peg -> (PegDiscState, [Maybe Move]) -> [(PegDiscState, [Maybe Move])]
tryPeg1toOtherPegs peg1 peg2 peg3  (pegState@(peg1Discs, peg2Discs, peg3Discs), movesTilNow@(Nothing:_)) = [(pegState ,movesTilNow)]
tryPeg1toOtherPegs peg1 peg2 peg3 (pegState@(peg1Discs, peg2Discs, peg3Discs), movesTilNow) =
  let
    peg1ToPeg2Moves  = if tryingToReverseMove peg1 peg2 movesTilNow then
                        movesTilNow
                       else
                        tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
    peg1ToPeg2Discs  =  if take 1 peg1ToPeg2Moves == [Nothing] then
                          pegState
                        else
                          (drop 1 peg1Discs, (head peg1Discs):peg2Discs, peg3Discs)
    peg1ToPeg3Moves  = if tryingToReverseMove peg1 peg3 movesTilNow then
                        movesTilNow
                       else
                        tryPeg1toPeg2 peg1 peg3 peg1Discs peg3Discs movesTilNow
    peg1ToPeg3Discs  =  if take 1 peg1ToPeg3Moves == [Nothing] then
                          pegState
                        else
                          (drop 1 peg1Discs, peg2Discs, (head peg1Discs):peg3Discs)
  in
    [(peg1ToPeg2Discs,peg1ToPeg2Moves), (peg1ToPeg3Discs, peg1ToPeg3Moves)]


tryPeg1toPeg2 :: Peg -> Peg -> PegDiscs -> PegDiscs -> [Maybe Move] -> [Maybe Move]
tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
  | null peg1Discs = Nothing:movesTilNow
  | null peg2Discs = Just (peg1,peg2):movesTilNow
  | head peg1Discs < head peg2Discs = Just (peg1,peg2):movesTilNow
  | otherwise = Nothing:movesTilNow

tryingToReverseMove :: Peg -> Peg -> [Maybe Move] -> Bool
tryingToReverseMove peg1 peg2 [] = False
tryingToReverseMove peg1 peg2 (headMove:_) = headMove == Just (peg2, peg1)
