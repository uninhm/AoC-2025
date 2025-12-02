import Parser

-- Parsing ---------------------

rangeParser :: Parser (Int, Int)
rangeParser = do
  a <- int
  char '-'
  b <- int
  return (a, b)

mainParser :: Parser [(Int, Int)]
mainParser = rangeParser `sepBy` char ','

-- Part 1 ----------------------

invalid :: Int -> Bool
invalid n = fstHalf == sndHalf
  where s = show n
        (fstHalf, sndHalf) = splitAt (length s `div` 2) s

part1 :: [(Int, Int)] -> Int
part1 = sum . concatMap f
 where f (a, b) = [ n | n <- [a..b], invalid n ]

-- Part 2 ----------------------

splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto 1 l = [l]
splitInto n l = firstChunck : splitInto (n-1) l'
  where chunckSize = length l `div` n
        (firstChunck, l') = splitAt chunckSize l

invalid2 :: Int -> Bool
invalid2 n = or [ allEq (splitInto i s) | i <- [2..length s] ]
  where s = show n
        allEq (h:l) = all (==h) l

part2 :: [(Int, Int)] -> Int
part2 = sum . concatMap f
 where f (a, b) = [ n | n <- [a..b], invalid2 n ]

-- Main ------------------------

main :: IO ()
main = do
  Just (ranges, _) <- parse mainParser <$> getLine
  putStr "Part 1: "
  print $ part1 ranges
  putStr "Part 2: "
  print $ part2 ranges
