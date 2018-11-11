module Battle where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Mob
import Item
import Level

data PlayerAction = Attack | Run | None deriving Eq
data MonsterAction = AttackM  deriving Eq

data BattleState = State PlayerAction MonsterAction
