import qualified Parser as P
import Data.Ix (inRange)
import Data.List (sort)

type Range = (Int, Int)

-- Parsing ------------------------------------

rangeP :: P.Parser Range
rangeP = do
  a <- P.int
  P.char '-'
  b <- P.int
  return (a, b)

mainParser :: P.Parser ([Range], [Int])
mainParser = do
  ranges <- P.some $ rangeP <* P.char '\n'
  P.char '\n'
  ids <- P.some $ P.int <* P.char '\n'
  return (ranges, ids)

-- Part 1 -------------------------------------

part1 :: [Range] -> [Int] -> Int
part1 ranges = length . filter (or . (map inRange ranges <*>) . pure)

-- Part 2 -------------------------------------

part2 :: [Range] -> Int
part2 ((a,b):(a',b'):ranges)
  | a' <= b = part2 $ (a, max b b') : ranges
  | otherwise = b-a+1 + part2 ((a', b') : ranges)
part2 [(a, b)] = b-a+1
part2 [] = 0

-- Main ---------------------------------------

main :: IO ()
main = do
  Just ((ranges, ids), _) <- P.parse mainParser <$> getContents
  putStr "Part 1: "
  print $ part1 ranges ids
  putStr "Part 2: "
  print $ part2 $ sort ranges
