module Item where

import Level
import Util

data Item = Item {
	name :: String,
	desc :: String,
	equipStats :: Maybe Stats,
  use :: Pos -> Level -> Level
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

drill :: Item
drill = Item {
  name = "drill",
  desc = "Your handy drill. Clears wreckage blocking your path.",
  equipStats = Nothing,
  use = usedrill
} where
  usedrill :: Pos -> Level -> Level
  usedrill (x,y) l = [[if m == x && n == y then Floor else l!!m!!n | m <- [0..length l - 1]] | n <- [0 .. length (head l) - 1]]
