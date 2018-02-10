module Parts.RectEnvelope (
  RectEnvelope(..)
  , buildRectEnvelope
  , computeRectEnvelopeAroundPlate
) where

import qualified Data.Glome.Vec as V
import Parts.Envelope
import Parts.Plate
import Parts.Switch
import Scad.Builders
import Scad.Sandwidge
import Scad
import qualified Scad.SphereConnectors as Sphere

data RectEnvelope = RectEnvelope {
  lf, lb, rb, rf :: V.Vec
  , r :: Double
  } deriving Show

buildPart :: Double -> Wall V.Vec -> V.Vec -> V.Vec -> ScadProgram
buildPart r wall a b = union $ [
  Sphere.connectPaths r (wallMiddle wall) [a, b]
  , hull [cube' a, cube' b]
  ]
  where
    cube' (V.Vec x y z) = translate x y z (cube0 (2*r) (2*r) (2*r))

buildRectEnvelope :: Plate -> RectEnvelope -> ScadProgram
buildRectEnvelope p (RectEnvelope lf lb rb rf r) = union $ [
  buildPart r (plateFrontWall p) lf rf
  , buildPart r (plateBackWall p) rb lb
  , buildPart r (plateLeftWall p) lb lf
  , buildPart r (plateRightWall p) rf rb
  ]

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
