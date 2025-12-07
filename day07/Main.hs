import qualified Data.Set as S
import Data.List (elemIndex, elemIndices)

import qualified Data.Map as M
import Data.Map.Internal (withoutKeys, restrictKeys)

-- Parsing -------------------------------------------

parse :: [String] -> (Int, [S.Set Int])
parse (firstLine : lns) = (i, sets)
  where Just i = elemIndex 'S' firstLine
        sets = map (S.fromList . elemIndices '^') lns

-- Part 1 --------------------------------------------

part1 :: S.Set Int -> [S.Set Int] -> Int
part1 beams [] = 0
part1 beams (ln:lns) =
  S.size splitted
    + part1 (S.union
              (beams S.\\ ln)
              (S.fromList $ [subtract 1, (+ 1)] <*> S.elems splitted))
            lns
  where splitted = S.intersection beams ln

-- Part 2 --------------------------------------------

part2 :: M.Map Int Int -> [S.Set Int] -> Int
part2 beams lns = sum $ M.elems $ go beams lns where
  go :: M.Map Int Int -> [S.Set Int] -> M.Map Int Int
  go beams [] = beams
  go beams (ln:lns) =
    go (M.unionWith (+)
             (beams `withoutKeys` ln)
             (M.unionWith (+) (M.mapKeys (subtract 1) splitted) (M.mapKeys (+1) splitted)))
       lns
    where splitted = beams `restrictKeys` ln

-- Main ----------------------------------------------

main :: IO ()
main = do
  (start, lns) <- parse . lines <$> getContents

  putStr "Part 1: "
  print $ part1 (S.singleton start) lns

  putStr "Part 2: "
  print $ part2 (M.singleton start 1) lns

