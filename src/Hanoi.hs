module Hanoi (hanoi) where

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

mapOverMoves = map (tryPeg1toOtherPegs "A" "B" "C" [1,2,3] [] []) ([Just [] :: Maybe [Move]])

tryPeg1toOtherPegs :: Peg -> Peg -> Peg -> PegsDiscs -> PegsDiscs -> PegsDiscs -> Maybe [Move] -> [Maybe [Move]]
tryPeg1toOtherPegs peg1 peg2 peg3 peg1Discs peg2Discs peg3Discs (Just movesTilNow) =
  let
    peg1ToPeg2Moves  = if tryingToReverseMove peg1 peg2 movesTilNow then
                        Nothing
                       else
                        tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
    pet1ToPeg3Moves  = if tryingToReverseMove peg1 peg3 movesTilNow then
                        Nothing
                       else
                        tryPeg1toPeg2 peg1 peg3 peg1Discs peg3Discs movesTilNow
  in
    [peg1ToPeg2Moves, pet1ToPeg3Moves]
tryPeg1toOtherPegs peg1 peg2 peg3 peg1Discs peg2Discs peg3Discs Nothing = [Nothing] :: [Maybe [Move]]

tryPeg1toPeg2 :: Peg -> Peg -> PegsDiscs -> PegsDiscs -> [Move] -> Maybe [Move]
tryPeg1toPeg2 peg1 peg2 peg1Discs peg2Discs movesTilNow
  | null peg1Discs = Nothing
  | null peg2Discs = Just ((peg1,peg2):movesTilNow)
  | head peg1Discs < head peg2Discs = Just ((peg1,peg2):movesTilNow)
  | otherwise = Nothing

tryingToReverseMove :: Peg -> Peg -> [Move] -> Bool
tryingToReverseMove peg1 peg2 moves
  | null moves = False
  | otherwise  = head moves == (peg2, peg1)
