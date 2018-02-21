-- | See "Keyboard.Parts.Envelope" to find out what envelope means.
--
-- This envelope is an extruded along z axis rectangle with side parallel to x
-- and y axis. Additional \"negative\"(or hole) figure is added below envelope
-- to ensure it's bottom surface is flat and doesn't merge with some other
-- figures.

module Keyboard.Parts.RectEnvelope (
  RectEnvelope(..)
  , rectEnvelopeSolid
  , rectEnvelopeHole
  , buildRectEnvelope
  , computeRectEnvelopeAroundPlate
) where

import qualified Data.Glome.Vec                  as V
import           Keyboard.Parts.Envelope
import           Keyboard.Parts.Plate
import           Keyboard.Parts.Switch
import           Keyboard.Scad
import           Keyboard.Scad.Primitives
import qualified Keyboard.Scad.Connectors.Sphere as Sphere
import           Keyboard.Scad.HollowFigure
import           Keyboard.Scad.Sandwidge

data RectEnvelope = RectEnvelope {
  lf, lb, rb, rf :: V.Vec
  , r            :: Double
  } deriving Show

buildPart :: Double -> Wall V.Vec -> V.Vec -> V.Vec -> ScadProgram
buildPart r wall a b = union $ [
  Sphere.connectPaths r (wallMiddle wall) [a, b]
  , hull [cube' a, cube' b]
  ]
  where
    cube' (V.Vec x y z) = translate x y z (cube0 (2*r) (2*r) (2*r))

rectEnvelopeSolid :: Plate -> RectEnvelope -> ScadProgram
rectEnvelopeSolid p (RectEnvelope lf lb rb rf r) = union $ [
  buildPart r (plateFrontWall p) lf rf
  , buildPart r (plateBackWall p) rb lb
  , buildPart r (plateLeftWall p) lb lf
  , buildPart r (plateRightWall p) rf rb
  ]

rectEnvelopeHole :: RectEnvelope -> ScadProgram
rectEnvelopeHole (RectEnvelope lf lb rb rf r) = union $ [
  part lf rf
  , part rb lb
  , part lb lf
  , part rf rb
  ]
  where
    part a b = hull ([cube' a, cube' b])
    cube' (V.Vec x y z) = translate x y (z - 2*r) (cube0 (2*r) (2*r) (2*r))

buildRectEnvelope :: Plate -> RectEnvelope -> HollowFigure
buildRectEnvelope plate rect =
  HollowFigure (Just $ rectEnvelopeSolid plate rect)
               (Just $ rectEnvelopeHole rect)

computeRectEnvelopeAroundPlate :: Plate -> Double -> Double -> Double
  -> RectEnvelope
computeRectEnvelopeAroundPlate plate x_len y_len radius =
  let (Rect rl rr rf rb) = plateBoundingRect plate
      cx = (rl + rr)/2
      cy = (rf + rb)/2
      l = cx - x_len/2
      r = cx + x_len/2
      f = cy - y_len/2
      b = cy + y_len/2
  in RectEnvelope (V.Vec l f 0) (V.Vec l b 0)
                  (V.Vec r b 0) (V.Vec r f 0)
                  radius
