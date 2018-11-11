module ItemData where

import Item
import Mob
import Level
import Util

drill :: Item
drill = Item {
  name = "drill",
  desc = "Your handy drill. Clears wreckage blocking your path.",
  equipStats = Nothing,
  use = usedrill
} where
  usedrill :: Pos -> World -> World
  usedrill (x,y) (Overworld l p ms) = [[if m == x && n == y then Floor else l!!m!!n | m <- [0..length l - 1]] | n <- [0 .. length (head l) - 1]]

medpack :: Item
medpack = Item {
  name = "medpack",
  desc = "A common medpack that heals 6 hp",
  equipStats = Nothing,
  use = usemedpack
} where
  usemedpack :: Pos -> World -> World
  usemedpack _ (Overworld l p ms) = (Overworld l (healMob 6 p) ms)
