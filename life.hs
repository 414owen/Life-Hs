import Data.List
import Control.Concurrent
import Debug.Trace
import System.Console.ANSI

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

rotateN :: Int -> [[a]] -> [[a]]
rotateN 0 lsts = lsts
rotateN n lsts | n < 0 || n >= 4 = rotateN (mod n 4) lsts
rotateN n lsts = rotateN (n - 1) (rotate lsts)

xToNum :: Char -> Int
xToNum x = if x == 'x' then 1 else 0

lifeCount (a:b:xs) = (xToNum a) : lifeCount (b:xs)
lifeCount _ = []

add2d :: Num a => [[a]] -> [[a]] -> [[a]]
add2d lsts1 lsts2 = zipWith (zipWith (+)) lsts1 lsts2

alive :: Char -> Int -> Char
alive 'x' n | n == 2 = 'x'
alive 'x' n | n == 3 = 'x'
alive '.' n | n == 3 = 'x'
alive  _  _          = '.'

nextBoard :: [[Char]] -> [[Int]] -> [[Char]]
nextBoard board neighbours = zipWith (\linea lineb -> zipWith alive linea lineb) board neighbours

zeroMat :: [[a]] -> [[Int]]
zeroMat lsts = map (map (const 0)) lsts

-- [1, 2, 3, 4] -> [4, 1, 2, 3, 4]
wraps :: [a] -> [a]
wraps lst = last lst : lst

neighboursInt (a:aa:as) bs = xToNum a + xToNum aa : neighboursInt (aa:as) bs
neighboursInt (a:[]) _ = []
neighboursInt _ _ = []

-- Takes displaced rows list
countNeighbours :: [[Char]] -> [[Int]]
countNeighbours lsts = zipWith neighboursInt lsts lsts

-- [[1, 2, 3], [2, 3, 4], [3, 4, 5]] ->
-- [[5, 3, 4, 5], [3, 1, 2, 3], [4, 2, 3, 4], [5, 3, 4, 5]]
shiftRows :: [[a]] -> [[a]]
shiftRows lsts = let newLists = map wraps lsts in
    last newLists : take (length newLists - 1) newLists

rotations :: [[a]] -> [Int] -> [[[a]]]
rotations lsts rots = map (\r -> rotateN r lsts) rots

neighbours :: [[Char]] -> [[Int]]
neighbours lsts = let rots = rotations lsts [0..3]
                      rotationShifts = map shiftRows rots -- [[[Char]]]
                      neighbourRots = map countNeighbours rotationShifts
                      neighboursUnRot = zipWith (flip rotateN) neighbourRots [0,(-1)..(-3)]
                      neighbours = foldl add2d (zeroMat lsts) neighboursUnRot
                  in neighbours

tick :: [[Char]] -> [[Char]]
tick lsts = nextBoard lsts (neighbours lsts)

life :: [[Char]] -> IO ()
life lsts = threadDelay 500000 >> clearScreen >> mapM_ putStrLn lsts >> life (tick lsts)

program :: IO [[Char]]
program = fmap lines (readFile "test.life")

main = program >>= life
