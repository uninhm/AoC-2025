parse :: String -> (Char, Int)
parse (x:s) = (x, read s)

part1 :: [(Char, Int)] -> Int
part1 = snd . foldl f (50, 0)
  where
    f (pos, cnt) (dir, dist)
      | newPos == 0 = (newPos, cnt+1)
      | otherwise   = (newPos, cnt)
        where newPos = if dir == 'L' then (pos - dist) `mod` 100
                       else (pos + dist) `mod` 100

part2 :: [(Char, Int)] -> Int
part2 = snd . foldl f (50, 0)
  where
    f (pos, cnt) (dir, dist)
      | pos == 0 = (newPos, cnt + dist `div` 100)
      | op pos dist <= 0 || 100 <= op pos dist = (newPos, cnt + 1 + diff `div` 100)
      | otherwise       = (newPos, cnt)
      where newPos = op pos dist `mod` 100
            op = if dir == 'L' then (-) else (+)
            diff = if dir == 'L' then dist - pos
                   else dist - (100 - pos)

main :: IO ()
main = do
  lns <- lines <$> getContents
  putStr "Part 1: "
  print $ part1 $ map parse lns
  putStr "Part 2: "
  print $ part2 $ map parse lns
