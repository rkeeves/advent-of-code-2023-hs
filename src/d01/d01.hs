import           Data.List  (find, isPrefixOf, tails)
import           Data.Maybe (mapMaybe)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap tails . lines

a :: [[String]] -> String
a = show . sum . mapMaybe (maybeHeadLast (\x y -> 10 * x + y) . mapMaybe (digits `lookupBy` isPrefixOf))

b :: [[String]] -> String
b = show . sum . mapMaybe (maybeHeadLast (\x y -> 10 * x + y) . mapMaybe ((digits ++ numbers) `lookupBy` isPrefixOf))

digits :: [(String, Int)]
digits = [(show x, x) | x <- [1 .. 9]]

numbers :: [(String, Int)]
numbers = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..]

lookupBy :: [(a, b)] -> (a -> a -> Bool) -> a -> Maybe b
lookupBy kvs p s = snd <$> find ((`p` s) . fst) kvs

maybeHeadLast :: (a -> a -> b) -> [a] -> Maybe b
maybeHeadLast _ [] = Nothing
maybeHeadLast f xs = Just $ f (head xs) (last xs)
