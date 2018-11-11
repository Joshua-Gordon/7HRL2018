module Screen where
import Data.Maybe
import Graphics.Gloss hiding (scale)
import Control.Monad
import Util
import Mob
import Level
import Prelude hiding(Right,Left)


squareSize :: Float
squareSize = 32

testSpace :: [[Tile]]
testSpace = [[Wall  , Floor , Wall ],
             [Floor , Floor , Floor],
             [Wall  , Floor , Wall ]]

drawLevel :: (Int,Int) -> [[Tile]] -> [Picture] -> Picture
drawLevel p tss as =  drawGrid $ map (map ((flip drawSpace) as)) tss

drawGrid :: [[Picture]] -> Picture
drawGrid xss = Pictures [translate (scale x) (scale y) p | ((x,y),p) <- concat $ grindex xss ]

scale :: Int -> Float
scale = fromIntegral . (*32)

grindex :: [[a]] -> [[((Int,Int),a)]]
grindex l = let wid = length l
                len = length (head l)
             in [ [((x,y), l!!x!!y ) | x <- [0..wid-1]] | y <- [0..len-1] ]

drawSpace :: Tile -> [Picture] -> Picture
drawSpace Wall  as = head as
drawSpace Floor as = head $ tail as

blkSq :: Picture
blkSq = color black $ rectangleSolid squareSize squareSize

orient :: Heading -> Picture -> Picture
orient Up    p = p
orient Down  p = rotate 180 p
orient Left  p = rotate (-90) p
orient Right p = rotate (90) p

drawM :: Monster -> [(String,Picture)] -> Picture
drawM m ps = let (x,y) = posM m in translate (scale x) (scale y) $ orient (headingM m) $ fromJust $ lookup (nameM m) ps

renderWorld :: World -> IO Picture
renderWorld (Overworld lv p ms) = do
  [floortile,player,alien,stairs] <- sequence $ map loadBMP ["floortile.bmp","player.bmp","alien.bmp","stairs.bmp"]
  let (px,py) = posP p
  let op = orient (heading p) player
  let pms = Pictures $ map (flip drawM [("Spaceman",alien),("stairs",stairs)] ) ms
  return $ Pictures [ translate (scale (-1*px)) (scale (-1*py)) $ Pictures [drawLevel (posP p) lv [blkSq,floortile],pms], op ]
renderWorld b = renderBattle b

renderBattle :: World -> IO Picture
renderBattle (Battle w p ms selected state turn) = do
  bg <- loadBMP "battlebackground.bmp"
  let rect = Color blue (rectangleWire 100 500)
  return $ Pictures [bg,case selected of
    (True,True) -> translate (-200) (-200) rect
    (True,False) -> translate 200 (-200) rect
    (False,True) -> translate (-200) (-500) rect
    (False,False) -> translate 200 (-500) rect]

main' = do
  lv <- genLevel 60 60
  alien <- spaceman (5,5)
  let testWorld = Overworld lv (startingPlayer{posP=(5,3),headingP = Right})  [alien{nameM="stairs"},alien]
  let testWorld = Overworld lv (startingPlayer{posP=(5,3),headingP = Right})  [alien]
  pic <- renderWorld testWorld
  display (InWindow "test" (1920,1080) (0,0)) red pic
