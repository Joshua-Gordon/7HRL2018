module Battle where

import Mob
import Item

data PlayerAction = Attack | Item Item | Ability Item | Run | None
data MonsterAction = AttackM | ItemM Item | Ability ItemM

data BattleState = State PlayerAction MonsterAction
