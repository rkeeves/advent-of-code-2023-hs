import           Data.Char (isAlphaNum)
import           Data.List (isSuffixOf)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . (\(s : _ : xs) -> (fmap (\c -> read [c]) s, fmap readEq xs)) . lines

a :: ([LR], [(String, (String, String))]) -> String
a (lrs, m) = show . length . takeWhile (/= "ZZZ") . nexts m (cycle lrs) $ "AAA"

b :: ([LR], [(String, (String, String))]) -> String
b (lrs, m) = show . foldr (lcm . length . takeWhile (not . ("Z" `isSuffixOf`)) . nexts m (cycle lrs)) 1 . filter ("A" `isSuffixOf`) . fmap fst $ m

data LR = L | R deriving (Eq, Show, Read)

nexts :: [(String, (String, String))] -> [LR] -> String -> [String]
nexts m lrs t0 = scanl (\t lr -> maybe t (\(l, r) -> if lr == L then l else r). lookup t $ m) t0 lrs

readEq :: String -> (String, (String, String))
readEq = (\[k, l, r] -> (k, (l, r))) . words . filter (\c -> isAlphaNum c || c == ' ')
