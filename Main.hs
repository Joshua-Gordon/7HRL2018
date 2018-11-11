import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Level
import Screen
import Mob




main :: IO ()
main = do
  lv' <- genLevel 33 59
  let lv = boundLevel lv'
  let alien = spaceman (5,5)
  let testWorld = Overworld lv startingPlayer [alien]
  playIO FullScreen red 1 testWorld renderWorld handleInput idstep

idstep :: Float -> World -> IO World
idstep _ w = return w
