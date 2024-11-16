import           Data.Char (ord)
import           Data.List (foldl')
import qualified Data.Map  as Map

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . splitBy (== ',') . head . lines

a :: [String] -> String
a = show . sum . fmap hash

b :: [String] -> String
b = show . sum . fmap score . Map.assocs . Map.map toBox . Map.fromListWith (flip (++)) . fmap ((\(k, v) -> (hash k, [(k, v)])) . readCmd)

type Command = (String, Maybe Int)
type Box = [(String, Int)]

hash :: String -> Int
hash = foldl' (\s c -> (17 * (s + ord c)) `rem` 256) 0

score :: (Int, Box) -> Int
score (n, box) = (n + 1) * sum [ i * v | (i, v) <- zip [1 ..] . fmap snd $ box]

toBox :: [Command] -> Box
toBox = foldl' (\box (k, m) -> maybe (remove k box) (\v -> insert k v box) m) []

remove :: String -> Box -> Box
remove k = filter ((/= k) . fst)

insert :: String -> Int -> Box -> Box
insert k v kvs
    | null kvs  = (k , v ) : []
    | k' == k   = (k , v ) : drop 1 kvs
    | otherwise = (k', v') : insert k v (drop 1 kvs)
    where (k', v') = head kvs

readCmd :: String -> Command
readCmd s
    | null v    = (k, Nothing)
    | otherwise = (k, Just (read v))
    where (k, _:v) = break (`elem` "=-") s

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p = foldr (\x (xs : xss) -> if p x then [] : (xs : xss) else (x : xs) : xss) [[]]
