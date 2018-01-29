module GeneralUtils (
  pairs
  , pairsLooped
) where

pairs :: [a] -> [(a, a)]
pairs xs = xs `zip` tail xs

pairsLooped :: [a] -> [(a, a)]
pairsLooped xs = pairs (xs ++ [head xs])
