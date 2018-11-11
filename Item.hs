module Item where

import Level
import Util
import {-# SOURCE #-} Mob

data Item = Item {
	name :: String,
	desc :: String,
	equipStats :: Maybe Stats,
  use :: Pos -> World -> World
	}

instance Eq Item where
  i1 == i2 = name i1 == name i2

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
