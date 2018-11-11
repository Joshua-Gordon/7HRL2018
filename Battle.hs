module Battle where

import Mob
import Item

data PlayerAction = Attack | Item Item | Ability Item | Run | None deriving Eq
data MonsterAction = AttackM | ItemM Item | AbilityM Item deriving Eq

data BattleState = State PlayerAction MonsterAction
