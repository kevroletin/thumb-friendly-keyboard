module Scad.Utils (
  dist
  , halfDist
  , centerMass
  , combineConvexShells
) where

import qualified Data.Glome.Vec as V
import           GeneralUtils
import           Scad.Builders
import           Scad.Internal

dist :: V.Vec -> V.Vec -> V.Flt
dist (V.Vec x y z) (V.Vec x' y' z') =
  sqrt((x - x') ** 2 + (y - y') ** 2 + (z - z') ** 2)

halfDist :: V.Vec -> V.Vec -> V.Flt
halfDist a b = (dist a b) / 2

centerMass :: [V.Vec] -> V.Vec
centerMass a@(x:xs) = V.vscale (foldr V.vadd x xs)
                                (1.0 / fromIntegral (length a))

combineConvexShells :: ScadProgram -> ScadProgram -> ScadProgram
combineConvexShells a b = union [
  intersection [a, b]
  , difference [
      union [a, b]
      , intersection [hull [a], hull [b]]
      ]
  ]
