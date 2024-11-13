import           Data.Bifunctor (bimap)
import           Data.List      (mapAccumL, sort)
import           Data.Maybe     (catMaybes)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . bothmap sort . unzip . xysWhichAre (== '#') . lines

a :: ([Int], [Int]) -> String
a = show . uncurry (+) . bothmap (sumDistances . zipWithSkips (\x skips -> x + skips) 0)

b :: ([Int], [Int]) -> String
b = show . uncurry (+) . bothmap (sumDistances . zipWithSkips (\x skips -> x + skips * (1000000 - 1)) 0)

bothmap f = bimap f f

sumDistances :: [Int] -> Int
sumDistances xs = let n = length xs in sum . zipWith (\i x -> (2 * i - n + 1) * x) [0 ..] $ xs

zipWithSkips :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
zipWithSkips f x0 = snd . mapAccumL (\(x, k) x' -> let k' = k + max (x' - x - 1) 0 in ((x', k'), f x' k')) (x0 - 1, 0)

xysWhichAre :: (Char -> Bool) -> [[Char]] -> [(Int, Int)]
xysWhichAre p css = concat [ catMaybes [ if p c then Just (x, y) else Nothing | (x, c) <- zip [0..] cs] | (y, cs) <- zip [0..] css]
