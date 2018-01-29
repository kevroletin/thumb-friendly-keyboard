module GeneralUtils (
  pairs
  , pairsLooped
) where


pairs :: [a] -> [(a, a)]
pairs xs = xs `zip` tail xs
-- pairs [] = []
-- pairs xs = drop 2 $ scanl (\(_, b) c -> (b, c)) (head xs, head xs) xs

pairsLooped :: [a] -> [(a, a)]
pairsLooped xs = pairs (xs ++ [head xs])
