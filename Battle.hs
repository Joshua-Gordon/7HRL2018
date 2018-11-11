module Battle where

import Mob
import Item

data BattleResult = Loss | Win [Item]

data Battle = Battle {
                player :: Player
              }

runBattle :: (Mob a) => a -> [a] -> IO BattleResult
runBattle player enemies = do
