import           Data.Bifunctor (first)
import           Data.IntMap    (IntMap)
import qualified Data.IntMap    as IntMap
import           Data.List      (elemIndex, mapAccumL, sortOn)
import           Data.Maybe     (fromMaybe, mapMaybe)
import           Data.Ord       (Down (Down))

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap ((\(s:n:_) -> (s, read n)) . words) . lines

a :: [(String, Int)] -> String
a = show . sum . zipWith (*) [1 ..] . fmap snd . sortOn (hand . frequency . fst) . sortOn fst . fmap (first (symbolsToNums order))
    where order = ['2' .. '9'] ++ ['T', 'J', 'Q', 'K', 'A']

b :: [(String, Int)] -> String
b = show . sum . zipWith (*) [1 ..] . fmap snd . sortOn (handWithJoker 0 . frequency . fst) . sortOn fst . fmap (first (symbolsToNums order))
    where order = ['J'] ++ ['2' .. '9'] ++ ['T', 'Q', 'K', 'A']

symbolsToNums :: [Char] -> String -> [Int]
symbolsToNums order = mapMaybe (`elemIndex` order)

frequency :: [Int] -> IntMap Int
frequency = IntMap.fromListWith (+) . fmap (, 1)

hand :: IntMap Int -> [Int]
hand = sortOn Down . filter (> 0) . fmap snd . IntMap.assocs

handWithJoker :: Int -> IntMap Int -> [Int]
handWithJoker j m = (\(x, xs) -> xs ++ [x]) . mapAccumL (\s x -> let x' = min n (x + s) in (s - (x' - x), x')) js . hand . IntMap.delete j $ m
    where
        n  = IntMap.foldl (+) 0 m
        js = fromMaybe 0 $ IntMap.lookup j m
