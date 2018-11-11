module Mob where

import Util
import {-# SOURCE #-} Level

data Player
data Monster


movePlayer :: Heading -> Player -> Level -> Player
getHeading :: Player -> Heading
getPos :: Player -> Pos
