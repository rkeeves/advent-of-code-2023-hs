import           Data.Bifunctor (first, second)
import           Data.Char      (isDigit)
import           Data.Either    (partitionEithers)
import           Data.Function  (on)
import           Data.List      (groupBy)
import           Text.Read      (readMaybe)

main :: IO ()
main = interact $ unlines . (\x -> ["A", a x, "B", b x]) . second (fmap (first (growBox 1))) . partitionEithers . concat . zipWith eitherSymsOrBoxesAtY [0..] . lines

a :: ([(Dot, Char)], [(Box, Int)]) -> String
a (dots, boxes) = show $ sum [ num | (box, num) <- boxes, any (`isInBox` box) syms ]
    where syms = fmap fst . filter ((/= '.') . snd) $ dots

b :: ([(Dot, Char)], [(Box, Int)]) -> String
b (dots, boxes) = show $ sum [ product ns | sym <- syms, let ns = [ n | (box, n) <- boxes, sym `isInBox` box ], length ns == 2]
    where syms = fmap fst . filter ((== '*') . snd) $ dots

data Dot = Dot { x :: Int, y :: Int }
data Box = Box { xlo :: Int, xhi :: Int, ylo :: Int, yhi :: Int }

growBox :: Int -> Box -> Box
growBox k (Box xlo xhi ylo yhi) = Box (xlo - k) (xhi + k) (ylo - k) (yhi + k)

isInBox :: Dot -> Box -> Bool
isInBox (Dot x y) (Box xlo xhi ylo yhi) = xlo <= x && x <= xhi && ylo <= y && y <= yhi

eitherSymsOrBoxesAtY :: Int -> [Char] -> [Either (Dot, Char) (Box, Int)]
eitherSymsOrBoxesAtY y =
    fmap ( (\(is, cs) -> maybe (Left (Dot (head is) y, head cs)) (Right . (Box (head is) (last is) y y,)) . readMaybe $ cs) . unzip )
    . groupBy ((&&) `on` (isDigit . snd))
    . zip [0..]
