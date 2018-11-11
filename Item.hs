module Item where

import Mob

data Item = Item {
	name :: String,
	desc :: String,
	equipStats :: Maybe Mob.Stats,
	use :: Mob.Mob -> Maybe Mob.Mob,
}

type Inv = [Item]
