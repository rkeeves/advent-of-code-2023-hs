import           Data.Bifunctor (bimap)
import           Data.List      (mapAccumL, sort)
import           Data.Maybe     (catMaybes)
import           Data.Ord       (Down (..))

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . bimap sort sort . unzip . xysWhichAre (== '#') . lines

a :: ([Int], [Int]) -> String
a = show . uncurry (+) . bothmap (sumDistances . zipWithSkips (\x skips -> x + skips))

b :: ([Int], [Int]) -> String
b = show . uncurry (+) . bothmap (sumDistances . zipWithSkips (\x skips -> x + skips * (1000000 - 1)))

bothmap f = bimap f f

sumDistances :: [Int] -> Int
sumDistances xs = let n = length xs in sum . zipWith (\i x -> (2 * i - n + 1) * x) [0 ..] $ xs

zipWithSkips :: (Int -> Int -> Int) -> [Int] -> [Int]
zipWithSkips _ [] = []
zipWithSkips f xs = snd . mapAccumL (\(x, k) x' -> let k' = k + max (x' - x - 1) 0 in ((x', k'), f x' k')) (head xs, 0) $ xs

xysWhichAre :: (Char -> Bool) -> [[Char]] -> [(Int, Int)]
xysWhichAre p css = concat [ catMaybes [ if p c then Just (x, y) else Nothing | (x, c) <- zip [0..] cs] | (y, cs) <- zip [0..] css]
