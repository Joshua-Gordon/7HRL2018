module Mob where

import Item
import Data.Maybe

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
	spd :: Int,
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
	Use Item.Item |
	Attack Mob

class Mob x where
	inv x :: Item.Inv
	pos x :: Pos
	act x :: Action

	baseStats x :: Stats
	baseStats x = defaultStats

	effStats x :: Stats
	effStats x = foldr (applyStats) (baseStats x) (catMaybe $ map equipStats $ inv x)
