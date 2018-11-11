module Util where

type Pos = (Int, Int)

zeroPos :: Pos
zeroPos = (0, 0)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = take n xs : chunk n (drop n xs)


data Heading = Up | Down | Left | Right

applyHeading :: Pos -> Heading -> Int -> Pos
applyHeading (x,y) Up d = (x, y+d)
applyHeading (x,y) Down d = (x, y-d)
applyHeading (x,y) Left d = (x-d, y)
applyHeading (x,y) Right d = (x+d, y)

headingOff :: Heading -> Int -> Pos
headingOff = applyHeading zeroPos
