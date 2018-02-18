{-|-- We assume next view location:
   -- + x goes from left to right
   -- + y goes from "near" to "far"
-}
module Scad (
  module Scad.Builders
  , Path
  , ScadProgram
  , Segment
  , V.vec
  , V.Vec(..)
  , renderToScad
  , Sandwidge(..)
  , Wall(..)
) where

import qualified Data.Glome.Vec as V
import           Scad.Builders
import           Scad.Internal
import           Scad.Path
import           Scad.Sandwidge
