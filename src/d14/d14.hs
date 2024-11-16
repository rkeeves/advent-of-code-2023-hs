import           Data.List (concatMap, groupBy, sort, transpose)
import           Data.Map  (Map)
import qualified Data.Map  as Map

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . Grid . lines

a :: Grid Char -> String
a = show . score . mapGrid N slide

b :: Grid Char -> String
b = show . score . predictAtN 1_000_000_000 (mapGrid E slide . mapGrid S slide . mapGrid W slide . mapGrid N slide)

newtype Grid a = Grid { unGrid :: [[a]] } deriving (Eq, Ord)
data Dir = N | W | S | E

score :: Grid Char -> Int
score = sum . zipWith (*) [1..] . fmap (length . filter (== 'O')) . reverse . unGrid

slide :: [Char] -> [Char]
slide = concatMap sort . groupBy (\a b -> a /= '#' && b /= '#')

mapGrid :: Dir -> ([a] -> [b]) -> Grid a -> Grid b
mapGrid N f = Grid . transpose . fmap (reverse . f . reverse) . transpose . unGrid
mapGrid W f = Grid . fmap (reverse . f . reverse) . unGrid
mapGrid S f = Grid . transpose . fmap f . transpose . unGrid
mapGrid E f = Grid . fmap f . unGrid

predictAtN :: Ord a => Int -> (a -> a) -> a -> a
predictAtN n f x0 = predictAtN' Map.empty n 0 f x0
    where
        predictAtN' :: Ord a => Map a Int -> Int -> Int -> (a -> a) -> a -> a
        predictAtN' m n i f x
            | n <= i                  = x
            | (Just i') <- m Map.!? x = iterate f x !! ((n - i') `rem` (i - i'))
            | otherwise               = predictAtN' (Map.insert x i m) n (i + 1)  f (f x)
