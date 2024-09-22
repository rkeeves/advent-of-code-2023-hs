import           Data.Bifunctor               (Bifunctor (second))
import           Data.Char                    (isDigit, isLetter)
import           Data.List                    (foldl')
import           Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S,
                                               satisfy, sepBy1, string)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . fmap (second (foldl' max 0 `ofKeys` ["red", "green", "blue"]) . parseGame) . lines

a :: [(Int, [Int])] -> String
a = show . sum . fmap fst . filter ((\[r, g, b] -> r <= 12 && g <= 13 && b <= 14) . snd)

b :: [(Int, [Int])] -> String
b = show . sum . fmap ((\[r, g, b] -> r * g * b) . snd)

ofKeys :: Eq a => ([b] -> c) -> [a] -> [(a, b)] -> [c]
ofKeys f ks kvs = [ f [ v | (k', v) <- kvs, k == k' ] | k <- ks]

parseGame :: String -> (Int, [(String, Int)])
parseGame = fst . head . filter (null . snd) . readP_to_S readGame

readGame :: ReadP (Int, [(String, Int)])
readGame = (\id cs -> (id, concat cs)) <$> gameTitle <*> (cubeSet `sepBy1` string "; ")
    where
        gameTitle = string "Game " *> num <* string ": "
        cubeSet   = cube `sepBy1` string ", "
        cube      = (\n c -> (c, n)) <$> num <* char ' ' <*> color
        color     = many1 (satisfy isLetter)
        num       = read <$> many1 (satisfy isDigit)
