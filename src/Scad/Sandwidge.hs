module Scad.Sandwidge (
  Sandwidge(..)
  , buildSadwidge
  , projectionDown
  , combineConvexShells
) where

import qualified Data.Glome.Vec as V
import Scad
import Scad.Builders
import GeneralUtils

data Sandwidge a = Sandwidge {
  sandwidgeTopPlane :: [a]
  , sandwidgeBottomPlane :: [a]
  } deriving Show

buildSadwidge (Sandwidge t0 b0) = buildPolyhedron $ do
  t <- mapM addVertex t0
  b <- mapM addVertex b0
  addSurface t
  addSurface (reverse b)
  eachSquare [t ++ [head t], b ++ [head b]] $ \a b c d ->
    addSurface [a, b, c ,d]
  return ()

middlePoint :: V.Vec -> V.Vec -> V.Vec
middlePoint (V.Vec x y z) (V.Vec x' y' z') =
  V.Vec ((x + x') / 2) ((y + y') / 2) ((z + z') / 2)

dist :: V.Vec -> V.Vec -> V.Flt
dist (V.Vec x y z) (V.Vec x' y' z') =
  sqrt((x - x') ** 2 + (y - y') ** 2 + (z - z') ** 2)

halfDist :: V.Vec -> V.Vec -> V.Flt
halfDist a b = (dist a b) / 2

sandwidgeMiddlePoints :: Sandwidge V.Vec -> [(V.Flt, V.Vec)]
sandwidgeMiddlePoints (Sandwidge t0 b0) =
  let go a b = (halfDist a b, middlePoint a b)
  in zipWith go (t0 ++ [head t0]) (b0 ++ [head b0])

addRoundBorders :: Sandwidge V.Vec -> ScadProgram
addRoundBorders = lineVarR . sandwidgeMiddlePoints

projectionDown :: Sandwidge V.Vec -> ScadProgram
projectionDown s@(Sandwidge t0 b0) = union $
  flip map (pairsLooped (t `zip` b)) $ \((a, b), (c, d)) ->
    (hull $ map (uncurry sphere) [a, b, c, d])
  where
    t = sandwidgeMiddlePoints s
    b = map lower t
    lower (r, V.Vec x y z) = (r, V.Vec x y r)

combineConvexShells :: ScadProgram -> ScadProgram -> ScadProgram
combineConvexShells a b = union [
  intersection [a, b]
  , difference [
      union [a, b]
      , intersection [hull [a], hull [b]]
      ]
  ]
