{-| Data types which represents a keyboards.
-}
module Keyboard (
  Switch(..)
  , Envelope(..)
  , TransformablePart(..)
) where

-- TODO: should we re-export?
import qualified Data.Glome.Vec as V
import Config

data Switch = Switch {
  pos :: V.Vec,
  angles :: V.Vec
  } deriving Show;

data Envelope = Envelope {
  left  :: [V.Vec],
  right :: [V.Vec],
  front :: [V.Vec],
  back  :: [V.Vec],
  upDirection :: V.Vec
  } deriving Show;

class TransformablePart a where
  translatePart :: V.Vec -> a -> a
  rotatePart :: V.Vec -> a -> a

instance TransformablePart Switch where
  translatePart = translateSwitch
  rotatePart = rotateSwitch

instance TransformablePart Envelope where
  translatePart = translateEnvelope
  rotatePart = rotateEnvelope

rotateVec :: V.Vec -> V.Vec -> V.Vec
rotateVec (V.Vec ax ay az) = V.xfm_point rotation
  where
    rotation = V.compose [V.rotate (V.vec 1 0 0) (V.deg ax)
                        , V.rotate (V.vec 0 1 0) (V.deg ay)
                        , V.rotate (V.vec 0 0 1) (V.deg az)]

translatePoint :: V.Vec -> V.Vec -> V.Vec
translatePoint delta = V.xfm_point (V.translate delta)

rotateSwitch :: V.Vec -> Switch -> Switch
rotateSwitch a (Switch pos angles) =
  Switch pos (rotateVec a angles)

translateSwitch :: V.Vec -> Switch -> Switch
translateSwitch d (Switch pos angles) =
  Switch (translatePoint d pos) angles

-- TODO: can paramentrize Envelope and use default Functor implementation
rotateEnvelope :: V.Vec -> Envelope -> Envelope
rotateEnvelope a0 (Envelope l r f b n) =
  Envelope (tr l) (tr r) (tr f) (tr b) (tr1 n)
  where
    tr1 = rotateVec a0
    tr = map tr1

translateEnvelope d (Envelope l r f b n) =
  Envelope (tr l) (tr r) (tr f) (tr b) (tr1 n)
  where
    tr1 = translatePoint d
    tr = map tr1
