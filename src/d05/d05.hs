import           Data.Char                    (isDigit, isLetter)
import           Data.List                    (find, foldl', nub, sort)
import           Text.ParserCombinators.ReadP (ReadP, char, many1, readP_to_S,
                                               satisfy, sepBy1, string)

main :: IO ()
main = interact $ unlines . (\x -> ["A", uncurry a x, "B", uncurry b x]) . parseSeedsAndDicts

a :: [Int] -> [Bijection] -> String
a seeds = show . minimum . fmap getLo . foldl' project (fmap (\l -> Rng l (l + 1)) seeds)

b :: [Int] -> [Bijection] -> String
b seeds = show . minimum . fmap getLo . foldl' project (pairsMap (\l n -> Rng l (l + n)) seeds)

newtype Bijection = Bijection { unBijection :: [Trf] } deriving (Show)
data Trf          = Trf { getRng :: Rng, getOffset :: Int }  deriving (Show)
data Rng          = Rng { getLo :: Int, getHi :: Int }  deriving (Show)

project :: [Rng] -> Bijection -> [Rng]
project rs b = concatMap (fmap (`translate` b) . (`subdivide` b)) rs
  where
    translate :: Rng -> Bijection -> Rng
    translate r@(Rng l h) = maybe r (\n -> Rng (l + n) (h + n)) . findOffset (\(Rng l' h') -> l' <= l && h <= h')

subdivide :: Rng -> Bijection -> [Rng]
subdivide (Rng l h) = nonorderedToRngs . ([l, h] ++) . concatMap ((filter (\x -> l <= x && x <= h) . (\r -> [getLo r, getHi r])) . getRng) . unBijection

nonorderedToRngs :: [Int] -> [Rng]
nonorderedToRngs = (\xs -> zipWith Rng xs (tail xs)) . sort . nub

findOffset :: (Rng -> Bool) -> Bijection -> Maybe Int
findOffset p = fmap getOffset . find (p . getRng) . unBijection

pairsMap :: (a -> a -> b) -> [a] -> [b]
pairsMap f (a : b : xs) = f a b : pairsMap f xs
pairsMap f _            = []

parseSeedsAndDicts :: String -> ([Int], [Bijection])
parseSeedsAndDicts = fst . head . filter (null . snd) . readP_to_S readSeedsAndDicts

readSeedsAndDicts :: ReadP ([Int], [Bijection])
readSeedsAndDicts = (,) <$> seeds <* eol <*> (dict `sepBy1` eol)
  where
    seeds    = string "seeds: " *> (num `sepBy1` space) <* eol
    dict     = (\_ xs -> Bijection xs) <$> dictName <*> many1 rng
    dictName = (,) <$> word <* string "-to-" <*> word <* string " map:" <* eol
    rng      = (\dst src n -> Trf (Rng src (src + n)) (dst - src)) <$> num <* space <*> num <* space <*> num <* eol
    word     = many1 (satisfy isLetter)
    num      = read <$> many1 (satisfy isDigit)
    space    = char ' '
    eol      = char '\n'
