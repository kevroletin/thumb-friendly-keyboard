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
import Data.Monoid
import GeneralUtils

data Sandwidge a = Sandwidge {
  sandwidgeTopPlane :: [a]
  , sandwidgeBottomPlane :: [a]
  } deriving Show

instance Monoid (Sandwidge a) where
  mempty = Sandwidge [] []
  mappend (Sandwidge a b) (Sandwidge c d) =
    Sandwidge (a `mappend` c) (b `mappend` d)

buildSadwidge (Sandwidge t0 b0) = buildPolyhedron $ do
  t <- mapM addVertex t0
  b <- mapM addVertex b0
  addSurface t
  addSurface (reverse b)
  eachSquare [t ++ [head t], b ++ [head b]] $ \a b c d ->
    addSurface [a, b, c ,d]
  return ()

segmentMiddlePoint :: V.Vec -> V.Vec -> V.Vec
segmentMiddlePoint (V.Vec x y z) (V.Vec x' y' z') =
  V.Vec ((x + x') / 2) ((y + y') / 2) ((z + z') / 2)

dist :: V.Vec -> V.Vec -> V.Flt
dist (V.Vec x y z) (V.Vec x' y' z') =
  sqrt((x - x') ** 2 + (y - y') ** 2 + (z - z') ** 2)

halfDist :: V.Vec -> V.Vec -> V.Flt
halfDist a b = (dist a b) / 2

sandwidgeMiddlePoints :: Sandwidge V.Vec -> [(V.Flt, V.Vec)]
sandwidgeMiddlePoints (Sandwidge t0 b0) =
  let go a b = (halfDist a b, segmentMiddlePoint a b)
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

-- a = V.Vec 0 0 0
-- b = V.Vec 10 0 0
-- c = V.Vec 0 10 0
-- xs = [V.Vec 0 0 0, V.Vec 10 0 0, V.Vec 20 0 0, V.Vec 30 20 0]
-- ys = [V.Vec 0 0 10, V.Vec 10 0 10, V.Vec 20 0 10, V.Vec 30 0 10]


triangle :: Double -> V.Vec -> V.Vec -> V.Vec -> ScadProgram
triangle r a b c =
  hull [
    sphere r a,
    sphere r b,
    sphere r c
  ]

middlePoint :: [V.Vec] -> V.Vec
middlePoint a@(x:xs) = V.vscale (foldr V.vadd x xs)
                                (1.0 / fromIntegral (length a))

square :: Double -> V.Vec -> V.Vec -> V.Vec -> V.Vec -> ScadProgram
square r a b c d =
  -- let m = VfromIntegral.vscale (foldr V.vadd a [b, c, d]) (1/4)
  let m = middlePoint [a, b, c, d]
  in union [
    triangle r a b m
    , triangle r b c m
    , triangle r c d m
    , triangle r d a m
    ]

-- test2 = square 3
--           (V.Vec (-10) (-10) 0)
--           (V.Vec (-10) (10) 0)
--           (V.Vec (10) (10) 0)
--           (V.Vec (10) (-10) 10)
-- connectLines r x@(x0 : x1 : []) (y0 : y1 : []) =
--   [triangle r x0 x1 y0, triangle r y0 y1 x1]
-- connectLines r x@(x0 : x1 : []) (y0 : y1 : ys) =
--   [triangle r x0 x1 y0, triangle r y0 y1 x1]
--   ++ connectLines r x (y1 : ys)
-- connectLines r (x0 : x1 : xs) y@(y0 : y1 : []) =
--   [triangle r x0 x1 y0, triangle r y0 y1 x1]
--   ++ connectLines r (x1 : xs) y
a = [V.Vec 0 0 0, V.Vec 100 0 0]
b = [V.Vec 0 0 10, V.Vec 50 10 10, V.Vec 100 0 10]

splitIntoProportionalSegments
  :: [V.Vec] -> [V.Vec] -> ([V.Vec], [V.Vec])
splitIntoProportionalSegments xs0 ys0 =
  let xLen = lineLength xs0
      yLen = lineLength ys0
      tooSmall x = x < 0.001
      go x'@(x0 : x1 : xs) y'@(y0 : y1 : ys) xRes yRes =
        let xSeg = (dist x0 x1) / xLen
            ySeg = (dist y0 y1) / yLen
        in
           if tooSmall $ abs (xSeg - ySeg) then
             go (x1 : xs) (y1 : ys) (x0 : xRes) (y0 : yRes)
           else if xSeg > ySeg then
             let m = linePoint (ySeg / xSeg) x0 x1
             in go (m : x1 : xs) (y1 : ys) (x0 : xRes) (y0 : yRes)
           else
             let m = linePoint (xSeg / ySeg) y0 y1
             in go (x1 : xs) (m : y1 : ys) (x0 : xRes) (y0 : yRes)
      go xs ys xRes yRes = (reverse xRes ++ xs, reverse yRes ++ ys)
  in go xs0 ys0 [] []

splitIntoEqualSegments :: V.Flt -> [V.Vec] -> [V.Vec]
splitIntoEqualSegments n xs0 =
  let len = (lineLength xs0) / n in splitWithStep len xs0

splitWithStep :: V.Flt -> [V.Vec] -> [V.Vec]
splitWithStep len xs0 =
  let go (x0 : x1 : xs) res =
        if dist x0 x1 < len then
          go (x1 : xs) (x0 : res)
        else
          let m = linePointLen len x0 x1
          in go (m : x1 : xs) (x0 : res)
      go xs res = reverse res ++ xs
  in go xs0 []

test = union $ connectLines 3
                (splitIntoEqualSegments 10 a)
                (splitIntoEqualSegments 10 b)

-- TODO: define orientation
connectTwoPaths :: [V.Vec] -> [V.Vec] -> PolyhedronMonad ()
connectTwoPaths a0 b0 = do
  let (xs0, ys0) = splitIntoProportionalSegments
                   (splitWithStep 0.5 a0)
                   (splitWithStep 0.5 b0)
  xs <- mapM addVertex xs0
  ys <- mapM addVertex ys0
  go (xs `zip` xs0) (ys `zip` ys0)
  where
      go x@((x0, x0') : (x1, x1') : xs) y@((y0, y0') : (y1, y1') : ys) = do
        m <- addVertex $ middlePoint [x0', x1', y0', y1']
        addSurface [x1, x0, m]
        addSurface [x0, y0, m]
        addSurface [y0, y1, m]
        addSurface [y1, x1, m]
        go (tail x) (tail y)
      go _ _ = return ()

connectLines' :: Double -> [V.Vec] -> [V.Vec] -> [ScadProgram]
connectLines' r (x0 : x1 : xs) (y0 : y1 : ys) =
  [square r x0 x1 y1 y0]
  -- [triangle r x0 x1 y0, triangle r y0 y1 x1]
  ++ connectLines' r (x1 : xs) (y1 : ys)
connectLines' _ _ _ = []

connectLines r xs0 ys0 =
  let (xs, ys) = splitIntoProportionalSegments xs0 ys0
  in connectLines' r xs ys

lineLength :: [V.Vec] -> V.Flt
lineLength xs = sum $ map (uncurry dist) (pairs xs)

{-|Unsafe code. Assumes a /= b and ratio in [0, 1] -}
linePoint :: V.Flt -> V.Vec -> V.Vec -> V.Vec
linePoint ratio a b = V.vscaleadd a (V.vsub b a) ratio

{-|Unsafe code. Assumes a /= b and ratio in [0, dist a b] -}
linePointLen :: V.Flt -> V.Vec -> V.Vec -> V.Vec
linePointLen len a b = V.vscaleadd a (V.vsub b a) (len / (dist a b))
