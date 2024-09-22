import           Data.Bifunctor (second)
import           Data.List      (intersect, uncons, unfoldr)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (length . uncurry intersect . readListPair) . lines

a :: [Int] -> String
a = show . sum . fmap (\n -> if n < 1 then 0 else 2 ^ (n - 1))

b :: [Int] -> String
b = show . sum . unfoldBy (\(n, copies) rest -> (copies, mapNextN n (second (+ copies)) rest)) . fmap (, 1)

mapNextN :: Int -> (a -> a) -> [a] -> [a]
mapNextN k f = zipWith (\i x -> if i < k then f x else x) [0 ..]

unfoldBy :: (a -> [a] -> (b, [a])) -> [a] -> [b]
unfoldBy f = unfoldr (fmap (uncurry f) . uncons)

readListPair :: String -> ([String], [String])
readListPair = second (drop 1) . break (== "|") . drop 2 . words
