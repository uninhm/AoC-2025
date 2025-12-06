import qualified Parser as P
import Data.List (transpose)

-- Parsing ----------------------------------------

lineP :: P.Parser [Int]
lineP = P.some (P.many (P.char ' ') *> P.int) <* P.many (P.char ' ') <* P.char '\n'

operationP :: P.Parser Char
operationP = P.char '*' P.<|> P.char '+'

mainP :: P.Parser [([Int], Char)]
mainP = do
  nums <- P.some lineP
  ops <- P.some $ operationP <* P.many (P.char ' ')
  return $ zip (transpose nums) ops

-- Part 1 -----------------------------------------

part1 :: [([Int], Char)] -> Int
part1 = sum . map f
 where
  f (l, '+') = sum l
  f (l, '*') = product l

-- Parsing 2 --------------------------------------

lineP2 :: P.Parser [Int]
lineP2 = do
  nums <- P.some (P.many (P.char ' ') *> P.int)
  P.many (P.char ' ')
  return nums

operationP2 :: P.Parser Char
operationP2 = P.char '*' P.<|> P.char '+'

mainP2 :: P.Parser [([Int], Char)]
mainP2 = P.some $ do
  nums <- lineP2
  op <- operationP2
  P.many (P.char ' ')
  return (nums, op)

-- Part 2 -----------------------------------------

part2 = part1

-- Main -------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  let Just (input, _) = P.parse mainP contents
  putStr "Part 1: "
  print $ part1 input

  let Just (input2, _) = P.parse mainP2 . concat . transpose . map reverse . lines $ contents
  putStr "Part 2: "
  print $ part2 input2
