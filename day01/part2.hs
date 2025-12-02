parse :: String -> (Char, Int)
parse (x:s) = (x, read s)

solve :: [(Char, Int)] -> Int
solve = snd . foldl f (50, 0)
  where
    f (pos, cnt) (dir, dist)
      | pos == 0 = (newPos, cnt + dist `div` 100)
      | op pos dist <= 0 || 100 <= op pos dist = (newPos, cnt + 1 + diff `div` 100)
      | otherwise       = (newPos, cnt)
      where newPos = (op pos dist) `mod` 100
            op = if dir == 'L' then (-) else (+)
            diff = if dir == 'L' then dist - pos
                   else dist - (100 - pos)

main :: IO ()
main = do
  lns <- lines <$> getContents
  print $ solve $ map parse lns
