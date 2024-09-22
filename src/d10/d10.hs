import           Data.Bifunctor (Bifunctor (bimap))
import           Data.List      (find, partition, sortOn, unfoldr)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromJust)
import           Data.Ord       (Down (Down))

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . bimap (fst . fromJust . find (\(_, c) -> c == 'S')) (Map.fromList . concatMap (uncurry pipe)) . partition (\(_, c) -> c == '.' || c == 'S') . readMatrix . lines

a :: (V2, Map Edge V2) -> String
a (start, graph) = show . (`div` 2) . length . longestPath graph $ start

b :: (V2, Map Edge V2) -> String
b (start, graph) = show .  (\es -> abs (polygonArea es) - (length es `div` 2) + 1) . longestPath graph $ start

type V2   = (Int, Int)
type Edge = (V2, V2)

polygonArea :: [Edge] -> Int
polygonArea edges = sum [ (x - x') * (y + y') | ((x, y), (x', y')) <- edges ] `div` 2

longestPath :: Map Edge V2 -> V2 -> [Edge]
longestPath m n = head . sortOn (Down . length) . fmap (path m) $ [(n, north n), (n, east n), (n, south n)]

path :: Map Edge V2 -> Edge -> [Edge]
path m start = (start : ) . takeWhile (/= start) . drop 1 . unfoldr (fmap (\e@(_, b) -> (e, fmap (b,) $ m Map.!? e))) $ Just start

pipe :: V2 -> Char -> [(Edge, V2)]
pipe x c
    | c == '|'  = sym north south
    | c == '-'  = sym east  west
    | c == 'L'  = sym north east
    | c == 'J'  = sym north west
    | c == '7'  = sym south west
    | c == 'F'  = sym south east
    | otherwise = []
    where
        sym fa fb = let a = fa x; b = fb x in [((a, x), b), ((b, x), a)]

north (x, y) = (x    , y - 1)
east (x, y)  = (x + 1, y)
south (x, y) = (x    , y + 1)
west (x, y)  = (x - 1, y)

readMatrix :: [[Char]] -> [(V2, Char)]
readMatrix xss = concat [ [ ((c, r), x)  | (c, x) <- zip [0 ..] xs ] | (r, xs) <- zip [0 ..] xss ]
