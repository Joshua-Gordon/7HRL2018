module Mob where

import Util
import Item

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
									posP = (0,0),
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
