module Battle where

data PlayerAction = Attack | Run | None
data MonsterAction = AttackM

data BattleState = State PlayerAction MonsterAction
