module Parts.Plate (
  Plate
  , buildPlate
  , plateFrontWall
  , plateBackWall
  , plateRightWall
  , plateLeftWall
) where

import qualified Data.Glome.Vec as V
import qualified Data.List as List
import Scad
import Scad.Builders
import Config
import Keyboard
import GeneralUtils
import Data.Foldable
import Parts.Switch
import Transformation
import Scad.Sandwidge

type Plate = [[Switch]]

data PolyhedronSocket = PolyhedronSocket {
  socketVertIds :: [Int]
  } deriving Show

socketVert :: Int -> PolyhedronSocket -> Int
socketVert n (PolyhedronSocket xs) = xs !! n

addSurfaceSandwich ::PolyhedronSurface -> PolyhedronMonad ()
addSurfaceSandwich v = do addSurface v
                          addSurface (reverse $ map topToBottom v)

addSwitch :: Switch -> PolyhedronMonad PolyhedronSocket
addSwitch switch = fmap PolyhedronSocket $ mapM addVertex (switchVertexes switch)

addTopBottomSurf :: PolyhedronSocket -> PolyhedronMonad ()
addTopBottomSurf sw =
  addSurfaceSandwich $ map (socketVertIds sw !!) [0, 1, 2, 3]

addLeftToRightSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addLeftToRightSurf l r =
  addSurfaceSandwich [socketVert 1 r, socketVert 0 r, socketVert 3 l, socketVert 2 l]

addFrontToBackSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addFrontToBackSurf n f =
  addSurfaceSandwich [socketVert 1 n, socketVert 0 f, socketVert 3 f, socketVert 2 n]

addMiddleSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addMiddleSurf s0 s1 s2 s3 =
  addSurfaceSandwich [socketVert 2 s0, socketVert 3 s1, socketVert 0 s2, socketVert 1 s3]

addWall :: Int -> Int -> [PolyhedronSocket] -> (PolyhedronSurface -> PolyhedronSurface) -> PolyhedronMonad ()
addWall a b xs f = do
  let topLine    = concat $ map (\s -> [socketVert a s, socketVert b s]) xs
  let bottomLine = concat $ map (\s -> [socketVert (topToBottom b) s, socketVert (topToBottom a) s]) (reverse xs)
  let res = topLine ++ bottomLine
  addSurface(f res)

addFrontWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addFrontWall sockets = addWall 0 3 (head sockets) id

addBackWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addBackWall sockets = addWall 1 2 (last sockets) reverse

addLeftWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addLeftWall sockets = addWall 0 1 (map head sockets) reverse

addRightWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addRightWall sockets = addWall 3 2 (map last sockets) id

buildPlateBody :: [[Switch]] -> ScadProgram
buildPlateBody switches = buildPolyhedron $ do
  sockets <- addSwitches switches
  for_ (concat sockets) addTopBottomSurf
  for_ sockets $ \line ->
    for_ (pairs line) (uncurry addLeftToRightSurf)
  for_ (List.transpose sockets) $ \row ->
    for_ (pairs row) (uncurry addFrontToBackSurf)
  eachSquare sockets addMiddleSurf
  addFrontWall sockets
  addBackWall sockets
  addLeftWall sockets
  addRightWall sockets
  return ()
  where
    addSwitches xs = sequence $ map (sequence . map addSwitch) xs

-- It works only for polyhedron. Be careful not to pass bottom vertexes.
-- Probably it should be a part of PolyhedronMonad.
topToBottom :: Int -> Int
topToBottom = (+ 4)

buildHoles :: [Switch] -> ScadProgram
buildHoles switches = union (map buildSwitchHole switches)

buildPlate :: Plate -> ScadProgram
buildPlate switches = difference [ buildPlateBody switches
                                 , buildHoles $ concat switches]

-- TODO: this functions are similar to add* functions used
-- with polyhedron monad. Refactor to use same logic.
plateFrontWall :: Plate -> Sandwidge V.Vec
plateFrontWall plate = Sandwidge
  (concat $ map (\x -> [x !! 0, x !! 3]) $ map switchVertexes (head plate))
  (concat $ map (\x -> [x !! 4, x !! 7]) $ map switchVertexes (head plate))

plateBackWall :: Plate -> Sandwidge V.Vec
plateBackWall plate = Sandwidge
  (concat $ map (\x -> [x !! 2, x !! 1]) $ reverse $ map switchVertexes (last plate))
  (concat $ map (\x -> [x !! 6, x !! 5]) $ reverse $ map switchVertexes (last plate))

plateRightWall :: Plate -> Sandwidge V.Vec
plateRightWall plate = Sandwidge
  (concat $ map (\x -> [x !! 3, x !! 2]) $ map switchVertexes (map last plate))
  (concat $ map (\x -> [x !! 7, x !! 6]) $ map switchVertexes (map last plate))

plateLeftWall :: Plate -> Sandwidge V.Vec
plateLeftWall plate = Sandwidge
  (concat $ map (\x -> [x !! 1, x !! 0]) $ reverse $ map switchVertexes (map head plate))
  (concat $ map (\x -> [x !! 5, x !! 4]) $ reverse $ map switchVertexes (map head plate))
