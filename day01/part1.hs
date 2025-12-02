parse :: String -> (Char, Int)
parse (x:s) = (x, read s)

solve :: [(Char, Int)] -> Int
solve = snd . foldl f (50, 0)
  where
    f (pos, cnt) (dir, dist)
      | newPos == 0 = (newPos, cnt+1)
      | otherwise   = (newPos, cnt)
        where newPos = if dir == 'L' then (pos - dist) `mod` 100
                       else (pos + dist) `mod` 100

main :: IO ()
main = do
  lns <- lines <$> getContents
  print $ solve $ map parse lns
