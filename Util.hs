module Util where

type Pos = (Int, Int)

zeroPos :: Pos
zeroPos = (0, 0)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = take n xs : chunk n (drop n xs)


data Heading = Up | Down | Left | Right

applyHeading :: Pos -> Heading -> Int -> Pos
applyHeading (x,y) Util.Up d = (x, y+d)
applyHeading (x,y) Util.Down d = (x, y-d)
applyHeading (x,y) Util.Left d = (x-d, y)
applyHeading (x,y) Util.Right d = (x+d, y)

headingOff :: Heading -> Int -> Pos
headingOff = applyHeading zeroPos
