main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . (\(t:d:_) -> (drop 1 $ words t , drop 1 $ words d)) . lines

a :: ([String], [String]) -> String
a = show . product . fmap (uncurry ways) . uncurry (zipWith (\t d -> (read t, read d)))

b :: ([String], [String]) -> String
b = show . uncurry ways . (\(ts, ds) -> (read $ concat ts, read $ concat ds))

ways :: Int -> Int -> Int
ways t d = let x = binarySearch 0 (t `div` 2) (\n -> n * (t - n) > d) in (t - 2 * x + 1)

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch l r p
    | l == r    = l
    | p m       = binarySearch l m p
    | otherwise = binarySearch (m + 1) r p
    where m = l + (r - l) `div` 2
