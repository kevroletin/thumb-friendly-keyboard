{-| Data types which represents a keyboards.
-}
module Keyboard (
  Switch(..)
  , Envelope(..)
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
  back  :: [V.Vec]
  } deriving Show;

