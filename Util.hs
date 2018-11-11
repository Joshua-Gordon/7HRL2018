module Util where

import System.Random

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

allHeadings :: [Heading]
allHeadings = [Util.Up, Util.Down, Util.Left, Util.Right]

choice :: StdGen -> [a] -> (a, StdGen)
choice s l = let
	ln = length l
	(idx, newS) = randomR (0,ln - 1) s
	in (l !! idx, newS)

replace :: [a] -> Int -> a -> [a]
replace xs n x = take (n-1) xs ++ [x] ++ drop n xs

remove :: [a] -> Int -> [a]
remove xs n = take (n-1) xs ++ drop n xs
