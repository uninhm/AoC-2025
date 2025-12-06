import Data.List (transpose)
import Data.Char (isDigit)

-- Part 1 -----------------------------------------

parse :: [String] -> ([Int], Char)
parse l = (map read (init l), last l !! 0)

-- Part 2 -----------------------------------------

parse2 :: [String] -> [([Int], Char)]
parse2 = go . concatMap words
  where
    go [] = []
    go (num:input)
      | num == "+" || num == "*" = ([], num !! 0) : go input
      | op == '+' || op == '*' = go $ init num : [op] : input
      | otherwise = (read num : nums', op') : t
        where op = last num
              ((nums', op'):t) = go input

-- Solution ---------------------------------------

solve :: [([Int], Char)] -> Int
solve = sum . map f
 where
  f (l, '+') = sum l
  f (l, '*') = product l


-- Main -------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  let input = map parse . transpose . map words . lines $ contents
  putStr "Part 1: "
  print $ solve input

  let input2 = parse2 . transpose . map reverse . lines $ contents
  putStr "Part 2: "
  print $ solve input2
