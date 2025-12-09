import Data.List (sortBy)
import qualified Parser as P

pairP :: P.Parser (Int, Int)
pairP = do
  a <- P.int
  P.char ','
  b <- P.int
  return (a, b)

mainP :: P.Parser [(Int, Int)]
mainP = pairP `P.sepBy` (P.char '\n')

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x, y) (x', y') = floor $ sqrt $ fromIntegral $ (x - x')^2 + (y - y')^2

area :: (Int, Int) -> (Int, Int) -> Int
area (x, y) (x', y') = (abs (x - x') + 1) * (abs(y - y') + 1)

part1 h w input = max (area tl br) (area tr bl)
  where
    cmpDist p a b = compare (dist p a) (dist p b)
    tl = head $ sortBy (cmpDist (1,1)) input
    tr = head $ sortBy (cmpDist (1,w)) input
    bl = head $ sortBy (cmpDist (h,1)) input
    br = head $ sortBy (cmpDist (h,w)) input

main :: IO ()
main = do
  Just (input, _) <- P.parse mainP <$> getContents
  let h = maximum $ map fst input
      w = maximum $ map snd input
  print $ part1 h w input

