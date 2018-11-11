module Mob where

import Util

import Data.Maybe

data Item = Item {
	name :: String,
	desc :: String,
	equipStats :: Maybe Stats,
	use :: Mob.Mob -> Maybe Mob,
}

type Inv = [Item]

data Equip {
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

data Action =
	Move Pos |
	Use Item |
	Attack Mob |
	Defend |
	NOP

class Mob where
	inv :: Inv
	pos :: Pos
	act :: Action
	heading :: Heading

	baseStats :: Stats
	baseStats = defaultStats

	effStats :: Stats
	effStats = foldr (applyStats) (baseStats x) (catMaybe $ map equipStats $ inv x)

data Player = Player {
							pos :: Pos,
							inv :: Inv,
							heading :: Heading,
							stats :: Stats
							}

startingPlayer :: Player
startingPlayer = Player {
									pos = (0,0),
									inv = [],
									heading = Util.Up,
									stats = defaultStats
									}

instance Player Mob where
	inv = inv,
	pos = pos,
	heading = heading,
	baseStats = stats
