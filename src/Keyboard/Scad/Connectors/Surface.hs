module Keyboard.Scad.Connectors.Surface (
  connectPaths
) where

import qualified Data.Glome.Vec         as V
import           Keyboard.GeneralUtils
import           Keyboard.Scad
import           Keyboard.Scad.Builders
import           Keyboard.Scad.Path
import           Keyboard.Scad.Utils

-- TODO: define orientation
connectPaths :: [V.Vec] -> [V.Vec] -> PolyhedronMonad ()
connectPaths a0 b0 = do
  let (xs0, ys0) = splitPathsIntoProportionalSegments
                   (splitPathWithStep 0.5 a0)
                   (splitPathWithStep 0.5 b0)
  xs <- mapM addVertex xs0
  ys <- mapM addVertex ys0
  go (xs `zip` xs0) (ys `zip` ys0)
  where
      go x@((x0, x0') : (x1, x1') : xs) y@((y0, y0') : (y1, y1') : ys) = do
        m <- addVertex $ centerMass [x0', x1', y0', y1']
        addSurface [x1, x0, m]
        addSurface [x0, y0, m]
        addSurface [y0, y1, m]
        addSurface [y1, x1, m]
        go (tail x) (tail y)
      go _ _ = return ()
