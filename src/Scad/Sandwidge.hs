module Scad.Sandwidge (
  Sandwidge(..)
  , buildSadwidge
  , sandwidgeMiddleWithR
  , sandwidgeMiddle
) where

import qualified Data.Glome.Vec as V
import Scad
import Scad.Builders
import Scad.Utils
import GeneralUtils
import Data.Monoid
import Scad.Path

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

sandwidgeMiddleWithR :: Sandwidge V.Vec -> [(V.Flt, V.Vec)]
sandwidgeMiddleWithR (Sandwidge t0 b0) =
  let go a b = (halfDist a b, segmentMiddlePoint a b)
  in zipWith go (t0 ++ [head t0]) (b0 ++ [head b0])

sandwidgeMiddle :: Sandwidge V.Vec -> [V.Vec]
sandwidgeMiddle (Sandwidge t0 b0) =
  zipWith segmentMiddlePoint (t0 ++ [head t0]) (b0 ++ [head b0])
