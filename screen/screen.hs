import Graphics.Gloss
import Level


squareSize :: Float
squareSize = 32

testSpace :: [[Tile]]
testSpace = [[Wall  , Floor , Wall ],
             [Floor , Floor , Floor],
             [Wall  , Floor , Wall ]]

render :: (Int,Int) -> [[Tile]] -> [Picture] -> Picture
render p tss as = translate (-29*32) (-16*32) 
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

main = do
  lv <- genLevel 59 33 
  floortile <- loadBMP "floortile.bmp"
  display (InWindow "Test" (1920,1080) (50,50) ) white $ render (29,17) lv [blkSq,floortile]
