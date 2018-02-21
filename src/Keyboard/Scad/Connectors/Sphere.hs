{-|Produce "3d lines" and "3d surfaces" by extruding spheres
-}
module Keyboard.Scad.Connectors.Sphere (
  addSandwigeBorders
  , projectSandwigeDown
  , projectWallDown
  , connectPaths
  , line
  , lineVarR
  , projectPathDown
) where

import qualified Data.Glome.Vec          as V
import           Keyboard.GeneralUtils
import           Keyboard.Scad
import           Keyboard.Scad.Primitives
import           Keyboard.Scad.Path
import           Keyboard.Scad.Sandwidge
import           Keyboard.Scad.Utils

addSandwigeBorders :: Sandwidge V.Vec -> ScadProgram
addSandwigeBorders = lineVarR . sandwidgeMiddleWithR

projectWallDown :: Wall V.Vec -> ScadProgram
projectWallDown s@(Wall t0 b0) = union $
  flip map (pairs (t `zip` b)) $ \((a, b), (c, d)) ->
    (hull $ map (uncurry sphere) [a, b, c, d])
  where
    t = wallMiddleWithR s
    b = map lower t
    lower (r, V.Vec x y z) = (r, V.Vec x y r)

projectSandwigeDown :: Sandwidge V.Vec -> ScadProgram
projectSandwigeDown s@(Sandwidge t0 b0) =
  projectWallDown (Wall (t0 ++ [head t0]) (b0 ++ [head b0]))

projectPathDown :: Double -> Path -> ScadProgram
projectPathDown r path = union $
  flip map (pairs path) $ \(a, b) ->
                            hull $ map (sphere r) [a, b, lower a, lower b]
  where
    lower (V.Vec x y z) = V.Vec x y r

triangle :: Double -> V.Vec -> V.Vec -> V.Vec -> ScadProgram
triangle r a b c =
  hull [
    sphere r a,
    sphere r b,
    sphere r c
  ]

square' :: Double -> V.Vec -> V.Vec -> V.Vec -> V.Vec -> ScadProgram
square' r a b c d =
  -- let m = VfromIntegral.vscale (foldr V.vadd a [b, c, d]) (1/4)
  let m = centerMass [a, b, c, d]
  in hull [
    sphere r a,
    sphere r b,
    sphere r c,
    sphere r d
    ]

square :: Double -> V.Vec -> V.Vec -> V.Vec -> V.Vec -> ScadProgram
square r a b c d =
  -- let m = VfromIntegral.vscale (foldr V.vadd a [b, c, d]) (1/4)
  let m = centerMass [a, b, c, d]
  in union [
    triangle r a b m
    , triangle r b c m
    , triangle r c d m
    , triangle r d a m
    ]

connectPaths' :: Double -> Path -> Path -> [ScadProgram]
connectPaths' r (x0 : x1 : xs) (y0 : y1 : ys) =
  -- [square r x0 x1 y1 y0]
  [square' r x0 x1 y1 y0]
  -- [triangle r x0 x1 y0, triangle r y0 y1 x1]
  ++ connectPaths' r (x1 : xs) (y1 : ys)
connectPaths' _ _ _ = []

connectPaths :: Double -> Path -> Path -> ScadProgram
connectPaths r xs0 ys0 =
  let (xs, ys) = splitPathsIntoProportionalSegments xs0 ys0
  in union $ connectPaths' r xs ys

line :: Double -> Path -> ScadProgram
line r xs = let pairs = xs `zip` (tail xs)
                segment (a, b) = hull [sphere r a, sphere r b]
            in union $ map segment pairs

lineVarR :: [(Double, V.Vec)] -> ScadProgram
lineVarR xs = let segment ((ra, a), (rb, b)) =
                    hull [sphere ra a, sphere rb b]
              in union $ map segment (pairs xs)
