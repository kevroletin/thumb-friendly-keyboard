module Keyboard.Scad.Path (
  Path
  , Segment
  , segmentMiddlePoint
  , pathLength
  , segmentPoint
  , segmentPointLen
  , splitPathIntoEqualSegments
  , splitPathWithStep
  , splitPathsIntoProportionalSegments
) where

import qualified Data.Glome.Vec         as V
import           Keyboard.GeneralUtils
import           Keyboard.Scad.Primitives
import           Keyboard.Scad.Utils

type Path = [V.Vec]
type Segment = (V.Vec, V.Vec)

segmentMiddlePoint :: V.Vec -> V.Vec -> V.Vec
segmentMiddlePoint (V.Vec x y z) (V.Vec x' y' z') =
  V.Vec ((x + x') / 2) ((y + y') / 2) ((z + z') / 2)

pathLength :: Path -> V.Flt
pathLength xs = sum $ map (uncurry dist) (pairs xs)

{-|Unsafe code. Assumes a /= b and ratio in [0, 1] -}
segmentPoint :: V.Flt -> Segment -> V.Vec
segmentPoint ratio (a, b) = V.vscaleadd a (V.vsub b a) ratio

{-|Unsafe code. Assumes a /= b and ratio in [0, dist a b] -}
segmentPointLen :: V.Flt -> Segment -> V.Vec
segmentPointLen len (a, b) = V.vscaleadd a (V.vsub b a) (len / (dist a b))

splitPathIntoEqualSegments :: V.Flt -> Path -> Path
splitPathIntoEqualSegments n xs0 =
  let len = (pathLength xs0) / n in splitPathWithStep len xs0

splitPathWithStep :: V.Flt -> Path -> Path
splitPathWithStep len xs0 =
  let go (x0 : x1 : xs) res =
        if dist x0 x1 < len then
          go (x1 : xs) (x0 : res)
        else
          let m = segmentPointLen len (x0, x1)
          in go (m : x1 : xs) (x0 : res)
      go xs res = reverse res ++ xs
  in go xs0 []

splitPathsIntoProportionalSegments
  :: Path -> Path -> (Path, Path)
splitPathsIntoProportionalSegments xs0 ys0 =
  let xLen = pathLength xs0
      yLen = pathLength ys0
      tooSmall x = x < 0.001
      go x'@(x0 : x1 : xs) y'@(y0 : y1 : ys) xRes yRes =
        let xSeg = (dist x0 x1) / xLen
            ySeg = (dist y0 y1) / yLen
        in
           if tooSmall $ abs (xSeg - ySeg) then
             go (x1 : xs) (y1 : ys) (x0 : xRes) (y0 : yRes)
           else if xSeg > ySeg then
             let m = segmentPoint (ySeg / xSeg) (x0, x1)
             in go (m : x1 : xs) (y1 : ys) (x0 : xRes) (y0 : yRes)
           else
             let m = segmentPoint (xSeg / ySeg) (y0, y1)
             in go (x1 : xs) (m : y1 : ys) (x0 : xRes) (y0 : yRes)
      go xs ys xRes yRes = (reverse xRes ++ xs, reverse yRes ++ ys)
  in go xs0 ys0 [] []
