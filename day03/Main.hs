-- Part 1 ------------------------------------

part1 :: [String] -> Int
part1 = sum . map f
  where
    f l = read $ m : [ maximum $ drop 1 $ dropWhile (/= m) l ]
      where m = maximum (init l)

-- Part 2 ------------------------------------

dropLast n = reverse . drop n . reverse

part2 :: [String] -> Int
part2 = sum . map (read . f 12)
  where
    f 0 _ = []
    f n l = m : f (n-1) (drop 1 $ dropWhile (/= m) l)
      where m = maximum (dropLast (n-1) l)

-- Main --------------------------------------

main :: IO ()
main = do
  input <- lines <$> getContents
  putStr "Part 1: "
  print $ part1 input
  putStr "Part 2: "
  print $ part2 input
