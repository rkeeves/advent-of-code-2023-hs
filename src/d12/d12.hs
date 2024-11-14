import           Data.Bifunctor (bimap)
import           Data.List      (foldl', intercalate, mapAccumL)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap readLine . lines

a :: [(String, [Int])] -> String
a = show . sum . fmap (uncurry countAccepts)

b :: [(String, [Int])] -> String
b = show . sum . fmap (uncurry countAccepts . bimap (intercalate ['?'] . replicate 5) (concat . replicate 5))

data NFARule = One (Char -> Bool) | Many (Char -> Bool)
type NFACell = (NFARule, Int)
type NFAPipeline = [NFACell]

countAccepts :: String -> [Int] -> Int
countAccepts s xs = snd $ last $ runPipeline (makePipeline xs) ('.' : s)

makePipeline :: [Int] -> NFAPipeline
makePipeline ns = [(dots, 1)] ++ fmap (,0) (intercalate [dots] ((`replicate` hash) <$> ns)) ++ [(dots, 0)]
    where dots = Many (/= '#')
          hash = One (/= '.')

runPipeline :: NFAPipeline -> String -> NFAPipeline
runPipeline = foldl' (\pipeline chr -> snd . mapAccumL (runCell chr) 0 $ pipeline)

runCell :: Char -> Int -> NFACell -> (Int, NFACell)
runCell c k (Many p, n) = if p c then (n, (Many p, n + k)) else (0, (Many p, k))
runCell c k (One p,  n) = if p c then (n, (One  p,     k)) else (0, (One  p, k))

readLine :: String -> (String, [Int])
readLine = (\[cs, s] -> (cs, read $ "[" ++ s ++ "]")) . words
