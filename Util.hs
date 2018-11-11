module Util where

type Pos = (Int, Int)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = take n xs : chunk n (drop n xs)


data Heading = Up | Down | Left | Right
