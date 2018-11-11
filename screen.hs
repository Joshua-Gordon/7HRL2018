import Graphics.Gloss
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
drawLevel p tss as = translate (-29*32) (-16*32) 
  $ let ntss = grab p tss in drawGrid $ map (map ((flip drawSpace) as)) ntss

grab :: (Int,Int) -> [[a]] -> [[a]]
grab (x,y) xss = (colGrab y) . (map (rowGrab x)) $ xss

rowGrab :: Int -> [a] -> [a]
rowGrab x xs = (drop (x - 29)) . (take 59) $ xs

colGrab :: Int -> [a] -> [a]
colGrab x xs = (drop (x - 17)) . (take 33) $ xs

drawGrid :: [[Picture]] -> Picture
drawGrid xss = Pictures [translate (scale y) (scale x) p | ((x,y),p) <- concat $ grindex xss ]
  where
    scale = fromIntegral . (*32) :: Int -> Float

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

renderWorld :: World -> IO Picture
renderWorld (Overworld lv p ms) = do
  [floortile,player,alien] <- sequence $ map loadBMP ["floortile.bmp","player.bmp","alien.bmp"]
  let op = orient (heading p) player
  return $ Pictures [drawLevel (29,17) lv [blkSq,floortile],op]

main = do
  lv <- genLevel 59 33 
  let testWorld = Overworld lv (startingPlayer{headingP = Right})  []
  pic <- renderWorld testWorld
  display (InWindow "test" (1920,1080) (0,0)) red pic



