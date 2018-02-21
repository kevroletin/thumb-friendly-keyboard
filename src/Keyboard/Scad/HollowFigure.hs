module Keyboard.Scad.HollowFigure (
  HollowFigure(..)
  , hollowFigureToScad
) where

import           Data.Maybe             (fromMaybe)
import           Data.Semigroup
import           Keyboard.Scad.Primitives (difference, dummyFigure, union)
import           Keyboard.Scad.Internal (ScadProgram (..))

data HollowFigure = HollowFigure {
  hollowFigureSolid  :: Maybe ScadProgram
  , hollowFigureHole :: Maybe ScadProgram
  } deriving Show

instance Monoid HollowFigure where
  mempty = HollowFigure Nothing Nothing
  mappend = concatHollowFigures

concatHollowFigures (HollowFigure a0 b0) (HollowFigure a1 b1) =
  HollowFigure (toScad a) (toScad b)
  where
    toList x = fromMaybe [] ((:[]) <$> x)
    toScad x = if null x then Nothing else Just (union x)
    a = toList a0 ++ toList a1
    b = toList b0 ++ toList b1

hollowFigureToScad :: HollowFigure -> ScadProgram
hollowFigureToScad (HollowFigure Nothing _)         = dummyFigure
hollowFigureToScad (HollowFigure (Just a) Nothing)  = a
hollowFigureToScad (HollowFigure (Just a) (Just b)) = difference [a, b]
