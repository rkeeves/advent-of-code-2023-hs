import           Data.Bifunctor (bimap)
import           Data.Bits      (Bits (popCount, xor))
import           Data.List      (foldl', transpose, uncons, unfoldr)
import           Data.Maybe     (mapMaybe)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . unzip . fmap (\block -> (readBitGrid block, readBitGrid $ transpose block)) . splitBy null . lines

a :: ([BitGrid], [BitGrid]) -> String
a = show . (\(horizontals, verticals) -> horizontals * 100 + verticals) . bothmap (sum . mapMaybe (findPositionByBitDifference 0))

b :: ([BitGrid], [BitGrid]) -> String
b = show . (\(horizontals, verticals) -> horizontals * 100 + verticals) . bothmap (sum . mapMaybe (findPositionByBitDifference 1))

type BitGrid = [Int]

findPositionByBitDifference :: Int -> BitGrid -> Maybe Int
findPositionByBitDifference d = lookup d . flip zip [1 ..] . zipLeftsRights (\l r -> sum $ zipWith bitDifference l r)

bitDifference :: Int -> Int -> Int
bitDifference x = popCount . xor x

zipLeftsRights :: ([a] -> [a] -> b) -> [a] -> [b]
zipLeftsRights f = drop 1 . unfoldr (\(ls, rrs) -> (\(r, rs) -> (f ls rrs, (r:ls, rs))) <$> uncons rrs) . ([],)

readBitGrid :: [String] -> BitGrid
readBitGrid = fmap (snd . foldl' (\(n, s) c -> (n * 2, s + if c == '.' then 0 else n)) (1, 0))

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr (\x (xs : xss) -> if p x then [] : (xs : xss) else (x : xs) : xss) [[]]

bothmap f = bimap f f
