module Transformation (
  Transformation(..)
  , Transformable(..)
  , transformBySeq
) where

import qualified Data.Glome.Vec as V

data Transformation = Rotate V.Vec
                    | Translate V.Vec
                    deriving Show

class Transformable a where
  transform :: Transformation -> a -> a

instance Transformable V.Vec where
  transform = transformVec

rotateVec :: V.Vec -> V.Vec -> V.Vec
rotateVec (V.Vec ax ay az) = V.xfm_point rotation
  where
    rotation = V.compose [V.rotate (V.vec 1 0 0) (V.deg ax)
                        , V.rotate (V.vec 0 1 0) (V.deg ay)
                        , V.rotate (V.vec 0 0 1) (V.deg az)]

translateVec :: V.Vec -> V.Vec -> V.Vec
translateVec delta = V.xfm_point (V.translate delta)

transformVec :: Transformation -> V.Vec -> V.Vec
transformVec (Rotate a) p    = rotateVec a p
transformVec (Translate d) p = translateVec d p

{-|Applies transformations in order similar to function
  application: from right to left.
-}
transformBySeq :: (Foldable t, Transformable b) => t Transformation -> b -> b
transformBySeq ts p = foldr transform p ts
