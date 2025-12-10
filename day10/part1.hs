import Parser
import Data.List (subsequences)
import Data.Bits ((.^.))
import Data.Maybe (isJust, fromJust)

lightsToInt :: String -> Int
lightsToInt = foldr (\c res -> 2*res + f c) 0 where
  f c = if c == '.' then 0 else 1

btnToInt :: [Int] -> Int
btnToInt = sum . map (2^)

lineP :: Parser (Int, [Int], [Int])
lineP = do
  char '['
  lights <- some $ char '.' <|> char '#'
  string "] "
  btns <- some $ char '(' *> (int `sepBy` char ',') <* string ") "
  char '{'
  joltages <- int `sepBy` char ','
  char '}'
  optional $ char '\n'
  return (lightsToInt lights, map btnToInt btns, joltages)

mainP :: Parser [(Int, [Int], [Int])]
mainP = some lineP

part1 :: [(Int, [Int], [Int])] -> Int
part1 = sum . map f where
  f (lights, btns, _) = minimum [ fromJust (calc 0 l) | l <- subsequences btns, isJust (calc 0 l) ]
    where
      calc cum _ | cum == lights = Just 0
      calc _ [] = Nothing
      calc cum (btn:l)
        | cum == lights = Just 0
        | otherwise = (1 +) <$> calc (cum .^. btn) l

main :: IO ()
main = do
  Just (input, _) <- parse mainP <$> getContents
  print $ part1 input
