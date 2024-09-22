import           Data.List (foldl')

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (fmap read . words) . lines

a :: [[Int]] -> String
a = show . sum . fmap extrapolate

b :: [[Int]] -> String
b = show . sum . fmap (extrapolate . reverse)

extrapolate :: [Int] -> Int
extrapolate = sum . foldl' (flip (scanl (-))) []
