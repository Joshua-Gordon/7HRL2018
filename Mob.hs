module Mob where

import Util
import Item
import {-# SOURCE #-} Level

import Data.Maybe

defaultStats :: Stats
defaultStats = Stats { hpmax = 10, hp = 10, atk = 10, def = 10, spd = 10 }

applyStats :: Stats -> Stats -> Stats
applyStats l r = Stats {
	hpmax = (hpmax l + hpmax r),
	hp = (hp l + hp r),
	atk = ((atk l) + (atk r)),
	def = ((def l) + (def r)),
	spd = ((spd l) + (spd r))
}



class Mob a where
	inv :: a -> Inv
	pos :: a -> Pos
	heading :: a -> Heading
	name :: a -> String

	baseStats :: a -> Stats
	baseStats _ = defaultStats

	effStats :: a -> Stats
	effStats x = foldr (applyStats) (baseStats x) (catMaybes $ map equipStats $ inv x)


healPlayer :: Int -> Player -> Player
healPlayer n m = let s = statsP m in m{statsP=s{hp=max (hpmax s) (n + hp s)}}

data Player = Player {
							nameP :: String,
							posP :: Pos,
							invP :: Inv,
							headingP :: Heading,
							statsP :: Stats
							}

startingPlayer :: Player
startingPlayer = Player {
									nameP = "bbrian",
									posP = (5,10),
									invP = [],
									headingP = Util.Up,
									statsP = defaultStats
									}

instance Mob Player where
	inv = invP
	pos = posP
	heading = headingP
	name = nameP
	baseStats = statsP

getHeading :: Player -> Heading
getHeading = headingP

getPos :: Player -> Pos
getPos = posP

data Monster = Monster {
							nameM :: String,
							descM :: String,
							posM :: Pos,
							invM :: Inv,
							headingM :: Heading,
							statsM :: Stats
}

instance Mob Monster where
	inv = invM
	pos = posM
	heading = headingM
	name = nameM
	baseStats = statsM

spaceman :: Pos -> Monster
spaceman p = Monster {
						nameM = "Spaceman",
						descM = "A little green man. How did he get here?",
						posM = p,
						invM = [],
						headingM = Util.Up,
						statsM = Stats {
														hpmax = 4,
														hp = 4,
														atk = 3,
														def = 4,
														spd = 6
													 }
}

movePlayer :: Heading -> Player -> Level -> Player
movePlayer h p l = let (x,y) = posP p in case h of
									Util.Up -> if l!!x!!(y+1) == Floor then p{posP=(x,y+1)} else p
									Util.Down -> if l!!x!!(y-1) == Floor then p{posP=(x,y-1)} else p
									Util.Left -> if l!!(x-1)!!y == Floor then p{posP=(x-1,y)} else p
									Util.Right -> if l!!(x+1)!!y == Floor then p{posP=(x+1,y)} else p

attackMob :: Player -> Monster -> Maybe Monster
attackMob p m = let s = statsP p
										s' = statsM m
										damage = (atk s - def s')
										hpcount = hp s'
								in if hpcount > damage then Just m{statsM=s'{hp=hpcount-damage}} else Nothing
