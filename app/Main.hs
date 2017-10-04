module Main where

import Control.Concurrent
import Data.List
import Debug.Trace
import System.Console.ANSI
import System.Environment
import System.IO

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

rotateN :: Int -> [[a]] -> [[a]]
rotateN 0 lsts = lsts
rotateN n lsts | n < 0 || n >= 4 = rotateN (mod n 4) lsts
rotateN n lsts = rotateN (n - 1) (rotate lsts)

xToNum :: Char -> Int
xToNum x = if x == 'x' then 1 else 0

add2d :: Num a => [[a]] -> [[a]] -> [[a]]
add2d = zipWith (zipWith (+))

alive :: Char -> Int -> Char
alive 'x' n | n == 2 = 'x'
alive  _  n | n == 3 = 'x'
alive  _  _          = '.'

nextBoard :: [String] -> [[Int]] -> [String]
nextBoard = zipWith (zipWith alive)

zeroMat :: [[a]] -> [[Int]]
zeroMat = map (map (const 0))

-- [1, 2, 3, 4] -> [4, 1, 2, 3, 4]
wraps :: [a] -> [a]
wraps lst = last lst : lst

neighboursInt :: String -> [Int]
neighboursInt (a:aa:as) = xToNum a + xToNum aa : neighboursInt (aa:as)
neighboursInt [a] = []
neighboursInt _ = []

-- Takes displaced rows list
countNeighbours :: [String] -> [[Int]]
countNeighbours = map neighboursInt

-- [[1, 2, 3], [2, 3, 4], [3, 4, 5]] ->
-- [[5, 3, 4, 5], [3, 1, 2, 3], [4, 2, 3, 4], [5, 3, 4, 5]]
shiftRows :: [[a]] -> [[a]]
shiftRows lsts = let newLists = map wraps lsts in
    last newLists : take (length newLists - 1) newLists

rotations :: [[a]] -> [Int] -> [[[a]]]
rotations lsts = map (`rotateN` lsts)

neighbours :: [String] -> [[Int]]
neighbours lsts = let rots = rotations lsts [0..3]
                      rotationShifts = map shiftRows rots -- [[String]]
                      neighbourRots = map countNeighbours rotationShifts
                      neighboursUnRot = zipWith (flip rotateN)
                          neighbourRots [0, 3, 2, 1]
                  in foldl add2d (zeroMat lsts) neighboursUnRot

tick :: [String] -> [String]
tick lsts = nextBoard lsts (neighbours lsts)

life :: [String] -> Int -> IO ()
life lsts delay = threadDelay delay >> clearScreen >>
    putStrLn (intercalate "\n" lsts) >> hFlush stdout >> life (tick lsts) delay

blockMode :: [String] -> IO ()
blockMode p = let chars = length p * (length (head p) + 2) + 9 in
    hSetBuffering stdout (BlockBuffering (Just chars))

program :: [String] -> IO ([String], Int)
program [a, b] = fmap (\c -> (lines c, read b)) (readFile a)
program [a] = fmap (\c -> (lines c, 100000)) (readFile a)
program _ = fmap (\c -> (lines c, 100000)) getContents

main = getArgs >>= program >>= (\(p, delay) -> blockMode p >> life p delay)
