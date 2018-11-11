module Mob where

import Util

import Data.Maybe

data Item = Item {
	name :: String,
	desc :: String,
	equipStats :: Maybe Stats,
	use :: Pos
	}

type Inv = [Item]

data Equip = Equip {
	lHand :: Maybe Item,
	rHand :: Maybe Item,
	chest :: Maybe Item,
	hat :: Maybe Item,
	lFoot :: Maybe Item,
	rFoot :: Maybe Item
}

data Stats = Stats {
	hpmax :: Int,
	hp :: Int,
	atk :: Int,
	def :: Int,
	spd :: Int
}

defaultStats :: Stats
defaultStats = Stats { hpmax = 10, hp = 10, atk = 10, def = 10, spd = 10 }

applyStats :: Stats -> Stats -> Stats
applyStats l r = Stats {
	atk = ((atk l) + (atk r)),
	def = ((def l) + (def r)),
	spd = ((spd l) + (spd r))
}



class Mob a where
	inv :: a -> Inv
	pos :: a -> Pos
	heading :: a -> Heading

	baseStats :: a -> Stats
	baseStats _ = defaultStats

	effStats :: a -> Stats
	effStats x = foldr (applyStats) (baseStats x) (catMaybes $ map equipStats $ inv x)


data Player = Player {
							posP :: Pos,
							invP :: Inv,
							headingP :: Heading,
							statsP :: Stats
							}

startingPlayer :: Player
startingPlayer = Player {
									posP = (0,0),
									invP = [],
									headingP = Util.Up,
									statsP = defaultStats
									}

instance Mob Player where
	inv = invP
	pos = posP
	heading = headingP
	baseStats = statsP

data Monster
