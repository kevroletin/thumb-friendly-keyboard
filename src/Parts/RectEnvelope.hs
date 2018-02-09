module Parts.RectEnvelope (
  RectEnvelope(..)
  , buildRectEnvelope
) where

import qualified Data.Glome.Vec as V
import Parts.Envelope
import Parts.Plate
import Scad.Builders
import Scad.Sandwidge
import Scad
import qualified Scad.SphereConnectors as Sphere

data RectEnvelope = RectEnvelope {
  lf, lb, rb, rf :: V.Vec
  , r :: Double
  } deriving Show;

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

-- TODO
buildCubeEnvelopeAroundPlate :: Plate -> Double -> Double
  -> Either String ScadProgram
buildCubeEnvelopeAroundPlate plate width height = undefined
