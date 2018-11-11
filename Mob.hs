module Mob where

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
	atk :: Int,
	def :: Int,
	spd :: Int
}

defaultStats :: Stats
defaultStats = Stats { atk = 10, def = 10, spd = 10 }

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
	Defend

class Mob where
	inv :: Inv
	pos :: Pos
	act :: Action
	heading :: Heading

	baseStats :: Stats
	baseStats = defaultStats

	effStats :: Stats
	effStats = foldr (applyStats) (baseStats x) (catMaybe $ map equipStats $ inv x)
