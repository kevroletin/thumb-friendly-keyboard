module Keyboard.Scad.Sandwidge (
  Sandwidge(..)
  , Wall(..)
  , buildSadwidge
  , sandwidgeMiddleWithR
  , sandwidgeMiddle
  , wallMiddle
  , wallMiddleWithR
  , reverseWall
) where

import qualified Data.Glome.Vec         as V
import           Data.Monoid
import           Keyboard.GeneralUtils
import           Keyboard.Scad.Primitives
import           Keyboard.Scad.Internal
import           Keyboard.Scad.Path
import           Keyboard.Scad.Utils

-- Sandwidge is looped
data Sandwidge a = Sandwidge {
  sandwidgeTopPlane      :: [a]
  , sandwidgeBottomPlane :: [a]
  } deriving Show

-- Wall is not looped
data Wall a = Wall {
  wallTopPlane      :: [a]
  , wallBottomPlane :: [a]
  } deriving Show

instance Monoid (Wall a) where
  mempty = Wall [] []
  mappend (Wall a b) (Wall c d) =
    Wall (a `mappend` c) (b `mappend` d)

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

sandwidgeMiddle :: Sandwidge V.Vec -> Path
sandwidgeMiddle (Sandwidge t0 b0) =
  zipWith segmentMiddlePoint (t0 ++ [head t0]) (b0 ++ [head b0])

wallMiddle :: Wall V.Vec -> Path
wallMiddle (Wall t0 b0) =
  zipWith segmentMiddlePoint t0 b0

wallMiddleWithR :: Wall V.Vec -> [(V.Flt, V.Vec)]
wallMiddleWithR (Wall t0 b0) =
  let go a b = (halfDist a b, segmentMiddlePoint a b)
  in zipWith go t0 b0

reverseWall :: Wall a -> Wall a
reverseWall (Wall t b) = Wall (reverse t) (reverse b)
