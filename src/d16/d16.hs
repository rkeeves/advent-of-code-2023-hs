import           Data.Bifunctor (second)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Maybe     (fromMaybe)
import           Data.Set       (Set)
import qualified Data.Set       as Set

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . readGraph . lines

a (Graph (x0, _ ) (y0, _ ) m) = show $ countReachables m ((x0, y0), E)

b (Graph (x0, x1) (y0, y1) m) = show . maximum . fmap (countReachables m) $ norths ++ souths ++ wests ++ easts
    where
        norths = (,N) . (,y1) <$> [x0 .. x1]
        souths = (,S) . (,y0) <$> [x0 .. x1]
        easts  = (,E) . (x0,) <$> [y0 .. y1]
        wests  = (,W) . (x1,) <$> [y0 .. y1]

type V2 = (Int, Int)
data Dir = N | E | S | W deriving (Eq, Ord, Show)
data Graph = Graph { getX0X1 :: (Int, Int), getY0Y1 :: (Int, Int), getEdges :: Map V2D [V2D] }
type V2D = (V2, Dir)

countReachables :: Map V2D [V2D] -> V2D -> Int
countReachables m = length . Set.map fst . closure (\v -> filter (`Map.member` m) . fromMaybe [] $ Map.lookup v m)

closure :: (Eq a, Ord a) => (a -> [a]) -> a -> Set a
closure f x0 = go ([x0], Set.singleton x0)
    where
        go (x:xs, s) = go . (\xs' -> (xs' ++ xs, foldr Set.insert s xs')) $ [ x' | x' <- f x, not $ x' `Set.member` s]
        go ([]  , s) = s

readGraph :: [[Char]] -> Graph
readGraph css = Graph (0, x1) (0, y1) . Map.fromListWith (++)
    . fmap (second (: []))
    . concat $ [ concat [ outgoings c (x, y) | (x, c) <- zip [0 ..] cs ] | (y, cs) <- zip [0 ..] css ]
    where
        x1 = length (head css) - 1
        y1 = length css - 1
        outgoings c v = fmap (\(d, d') -> ((v, d), (neighbor d' v, d'))) . connections $ c

connections :: Char -> [(Dir, Dir)]
connections '.'  = [(N, N), (S, S), (E, E), (W, W)]
connections '/'  = [(N, E), (S, W), (E, N), (W, S)]
connections '\\' = [(N, W), (S, E), (E, S), (W, N)]
connections '|'  = [(N, N), (S, S), (E, N), (E, S), (W, N), (W, S)]
connections '-'  = [(N, E), (N, W), (S, E), (S, W), (E, E), (W, W)]

neighbor :: Dir -> V2 -> V2
neighbor N (x, y) = (x  ,y-1)
neighbor E (x, y) = (x+1,y  )
neighbor S (x, y) = (x  ,y+1)
neighbor W (x, y) = (x-1,y  )
