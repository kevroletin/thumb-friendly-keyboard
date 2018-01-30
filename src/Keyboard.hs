{-| Data types which represents a keyboards.
-}
module Keyboard (
  Envelope(..)
) where

-- TODO: should we re-export?
import qualified Data.Glome.Vec as V
import Transformation
import Config
import Parts.Switch

data Envelope = Envelope {
  left  :: [V.Vec],
  right :: [V.Vec],
  front :: [V.Vec],
  back  :: [V.Vec],
  upDirection :: V.Vec
  } deriving Show;

-- instance Transformable Envelope where
--   transform (Translate d) = translateEnvelope d
--   transform (Rotate a) = rotateEnvelope a


-- rotateEnvelope :: V.Vec -> Envelope -> Envelope
-- rotateEnvelope a0 (Envelope l r f b n) =
--   Envelope (tr l) (tr r) (tr f) (tr b) (tr1 n)
--   where
--     tr1 = rotateVec a0
--     tr = map tr1

-- translateEnvelope d (Envelope l r f b n) =
--   Envelope (tr l) (tr r) (tr f) (tr b) (tr1 n)
--   where
--     tr1 = translatePoint d
--     tr = map tr1
