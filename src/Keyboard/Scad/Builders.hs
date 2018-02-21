module Keyboard.Scad.Builders (
  block
  , color
  , cube
  , cube0
  , difference
  , dummyFigure
  , hull
  , intersection
  , rotate
  , sphere
  , translate
  , union

  , PolyhedronSurface
  , PolyhedronMonad
  , addVertex
  , addSurface
  , getCurrentPolyhedron
  , buildPolyhedron
  -- TODO: make general and move out from this module
  , eachSquare
) where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Glome.Vec          as V
import           Keyboard.GeneralUtils
import           Keyboard.Scad.Internal
import           Keyboard.Transformation

type PolyhedronMonad a = State (Int, [V.Vec], [PolyhedronSurface]) a

instance Transformable ScadProgram where
  transform (Rotate angle) p = rotateV angle p
  transform (Translate d) p  = translateV d p

eachSquare :: [[a]]
           -> (a -> a -> a -> a -> PolyhedronMonad b)
           -> PolyhedronMonad ()
eachSquare sockets f = do
  for_ [0 .. (height - 2)] $ \y' ->
    for_ [0 .. (width - 2)] $ \x' -> do
        f (sockets !! y' !! x') (sockets !! (y' + 1) !! x') (sockets !! (y' + 1) !! (x' + 1)) (sockets !! y' !! (x' + 1))
  where
    width = length (head sockets)
    height = length sockets

addVertex :: V.Vec -> PolyhedronMonad Int
addVertex v = do (idx, vert', surf) <- get
                 put (idx + 1, vert' ++ [v], surf)
                 return idx

addSurface :: PolyhedronSurface -> PolyhedronMonad ()
addSurface surf = do (idx, vert', sx) <- get
                     put (idx, vert', sx ++ [surf])
                     return ()

getCurrentPolyhedron :: PolyhedronMonad ScadProgram
getCurrentPolyhedron = do (_, verts, surfs) <- get
                          return $ Polyhedron verts surfs

buildPolyhedron :: PolyhedronMonad a -> ScadProgram
buildPolyhedron m = evalState (m >> getCurrentPolyhedron) (0, [], [])

hull :: [ScadProgram] -> ScadProgram
hull xs = Operator "hull" NoParams (Block xs)

union :: [ScadProgram] -> ScadProgram
union xs = Operator "union" NoParams (Block xs)

intersection :: [ScadProgram] -> ScadProgram
intersection xs = Operator "intersection" NoParams (Block xs)

difference :: [ScadProgram] -> ScadProgram
difference xs = Operator "difference" NoParams (Block xs)

cube :: V.Flt -> V.Flt -> V.Flt -> ScadProgram
cube x y z = Cube (V.Vec x y z)

cube0 :: V.Flt -> V.Flt -> V.Flt -> ScadProgram
cube0 x y z = translate (-x/2) (-y/2) (-z/2) $ Cube (V.Vec x y z)

translate :: V.Flt -> V.Flt -> V.Flt -> ScadProgram -> ScadProgram
translate x y z = Operator "translate" (VecParams $ V.Vec x y z)

translateV :: V.Vec -> ScadProgram -> ScadProgram
translateV shift = Operator "translate" (VecParams shift)

rotate :: V.Flt -> V.Flt -> V.Flt -> ScadProgram -> ScadProgram
rotate x y z =  Operator "rotate" (VecParams $ V.Vec x y z)

rotateV :: V.Vec -> ScadProgram -> ScadProgram
rotateV angle =  Operator "rotate" (VecParams angle)

color :: String -> ScadProgram -> ScadProgram
color c = Operator "color" (StringParams c)

dummyFigure :: ScadProgram
dummyFigure = Block []

block :: [ScadProgram] -> ScadProgram
block = Block

sphere :: Double -> V.Vec -> ScadProgram
sphere r pos = translateV pos $ Sphere r
