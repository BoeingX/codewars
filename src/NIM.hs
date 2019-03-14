{- See: https://www.codewars.com/kata/54120de842dff35232000195/train/haskell

This kata explores writing an AI for a two player, turn based game called NIM.
The Board

The board starts out with several piles of straw. Each pile has a random number of straws.

Pile 0: ||||

Pile 1: ||

Pile 2: |||||

Pile 3: |

Pile 4: ||||||

...or more concisely: [4,2,5,1,6]

The Rules

    The players take turns picking a pile, and removing any number of straws from the pile they pick
    A player must pick at least one straw
    If a player picks the last straw, she wins!

The Task

In this kata, you have to write an AI to play the straw picking game.

You have to encode an AI in a function choose_move (or chooseMove, or choose-move) that takes a board, represented as a list of positive integers, and returns

(pileIndex, numberOfStraws)

Which refers to an index of a pile on the board, and some none-zero number of straws to draw from that pile.

The test suite is written so that your AI is expected to play 50 games and win every game it plays.
-}
module NIM where

import Data.Bits

nimSum :: [Int] -> Int
nimSum = foldr xor 0

-- | See: https://en.wikipedia.org/wiki/Nim
chooseMove :: [Int] -> (Int,Int)
chooseMove xs = head $ filter ((> 0) . snd) $ zip [0..] $ map (\x -> x - (x `xor` nimsum)) xs
    where nimsum = nimSum xs
