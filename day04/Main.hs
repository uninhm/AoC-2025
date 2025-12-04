import Data.Array.IArray
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import qualified Data.Set as S

countAround ::  S.Set (Int, Int) -> (Int, Int) -> Int
countAround a (i, j) =
  length $ filter (`S.member` a)
    [ (i+di, j+dj) | di <- [-1..1], dj <- [-1..1]
                   , di /= 0 || dj /= 0
    ]

part1 :: S.Set (Int, Int) -> Int
part1 a = length . filter ((< 4) . countAround a) $ S.elems a

-- Part 2 ---------------------------------------

clean :: S.Set (Int, Int) -> (Int, S.Set (Int, Int))
clean a = (S.size a - S.size a', a')
  where a' = S.filter ((>= 4) . countAround a) a

part2 :: S.Set (Int, Int) -> Int
part2 a
  | n > 0     = n + part2 a'
  | otherwise = n
    where (n, a') = clean a

-- Main -----------------------------------------

main :: IO ()
main = do
  lns <- lines <$> getContents

  let
    h = length lns
    w = length (head lns)
    input = listArray ((1,1),(h,w)) $ concat lns :: Array (Int, Int) Char
    inputSet = S.fromList $ filter ((=='@') . (input!)) $ indices input

  putStr "Part 1: "
  print $ part1 inputSet
  putStr "Part 2: "
  print $ part2 inputSet
