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
