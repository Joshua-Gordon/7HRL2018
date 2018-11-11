module Battle where

import Mob
import Item

data BattleResult = Loss | Win [Item]

runBattle :: (Mob a) => a -> [a] -> BattleResult
