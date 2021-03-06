-- | This picture describes how sockets are connected together:
-- <<doc/img/socket_orientation.png VertexesOrientation>>

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Keyboard.Parts.Plate (
  Plate
  , Rect(..)
  , plateBoundingRect
  , plateSolid
  , plateHoles
  , buildPlate
  , plateFrontWall
  , plateBackWall
  , plateRightWall
  , plateLeftWall
  , platePerimeter
) where
import           Data.Foldable
import qualified Data.Glome.Vec             as V
import qualified Data.List                  as List
import           Data.Monoid
import           Keyboard.Config
import           Keyboard.GeneralUtils
import           Keyboard.Parts.Switch
import           Keyboard.Scad
import           Keyboard.Scad.HollowFigure
import           Keyboard.Scad.Primitives
import           Keyboard.Scad.Sandwidge
import           Keyboard.Transformation

type Plate = [[Switch]]

instance Transformable Plate where
  transform t = fmap (fmap (transform t))

{-|Parallelepiped where we create hole to turn it into a socket for a key. It
  contains 8 "vertexes". But since it is supposed to be used within a polyhedron
  instead of actual coordinates it holds ids.
-}
data SocketBar = SocketBar {
  socketVertIds :: [Int]
  } deriving Show

data Rect = Rect {
  rectLeft    :: Double
  , rectRight :: Double
  , rectFront :: Double
  , rectBack  :: Double
  } deriving Show

plateBoundingRect :: Plate -> Rect
plateBoundingRect plate =
  let switches = concat plate
      l = minimum $ map V.x $ concat $ map switchVertexes switches
      r = maximum $ map V.x $ concat $ map switchVertexes switches
      f = minimum $ map V.y $ concat $ map switchVertexes switches
      b = maximum $ map V.y $ concat $ map switchVertexes switches
  in (Rect l r f b)

socketVert :: Int -> SocketBar -> Int
socketVert n (SocketBar xs) = xs !! n

addSurfaceSandwich ::PolyhedronSurface -> PolyhedronMonad ()
addSurfaceSandwich v = do addSurface v
                          addSurface (reverse $ map topToBottom v)

addSwitch :: Switch -> PolyhedronMonad SocketBar
addSwitch switch = fmap SocketBar $ mapM addVertex (switchVertexes switch)

addTopBottomSurf :: SocketBar -> PolyhedronMonad ()
addTopBottomSurf sw =
  addSurfaceSandwich $ map (socketVertIds sw !!) [0, 1, 2, 3]

addLeftToRightSurf :: SocketBar -> SocketBar -> PolyhedronMonad ()
addLeftToRightSurf l r =
  addSurfaceSandwich [socketVert 1 r, socketVert 0 r, socketVert 3 l, socketVert 2 l]

addFrontToBackSurf :: SocketBar -> SocketBar -> PolyhedronMonad ()
addFrontToBackSurf n f =
  addSurfaceSandwich [socketVert 1 n, socketVert 0 f, socketVert 3 f, socketVert 2 n]

addMiddleSurf :: SocketBar -> SocketBar -> SocketBar -> SocketBar -> PolyhedronMonad ()
addMiddleSurf s0 s1 s2 s3 =
  addSurfaceSandwich [socketVert 2 s0, socketVert 3 s1, socketVert 0 s2, socketVert 1 s3]

addWall :: Int -> Int -> [SocketBar] -> (PolyhedronSurface -> PolyhedronSurface) -> PolyhedronMonad ()
addWall a b xs f = do
  let topLine    = concat $ map (\s -> [socketVert a s, socketVert b s]) xs
  let bottomLine = concat $ map (\s -> [socketVert (topToBottom b) s, socketVert (topToBottom a) s]) (reverse xs)
  let res = topLine ++ bottomLine
  addSurface(f res)

addFrontWall :: [[SocketBar]] -> PolyhedronMonad ()
addFrontWall sockets = addWall 0 3 (head sockets) id

addBackWall :: [[SocketBar]] -> PolyhedronMonad ()
addBackWall sockets = addWall 1 2 (last sockets) reverse

addLeftWall :: [[SocketBar]] -> PolyhedronMonad ()
addLeftWall sockets = addWall 0 1 (map head sockets) reverse

addRightWall :: [[SocketBar]] -> PolyhedronMonad ()
addRightWall sockets = addWall 3 2 (map last sockets) id

plateSolid :: [[Switch]] -> ScadProgram
plateSolid switches = buildPolyhedron $ do
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

plateHoles :: Plate -> ScadProgram
plateHoles switches = union (map switchHole (concat switches)
                              ++ map keycapPadHole (concat switches))

buildPlate :: Plate -> HollowFigure
buildPlate p = HollowFigure (Just $ plateSolid p) (Just $ plateHoles p)

-- TODO: this functions are similar to add* functions used
-- with polyhedron monad. Refactor to use same logic.
plateFrontWall :: Plate -> Wall V.Vec
plateFrontWall plate = Wall
  (concat $ map (\x -> [x !! 0, x !! 3]) $ map switchVertexes (head plate))
  (concat $ map (\x -> [x !! 4, x !! 7]) $ map switchVertexes (head plate))

plateBackWall :: Plate -> Wall V.Vec
plateBackWall plate = Wall
  (concat $ map (\x -> [x !! 2, x !! 1]) $ reverse $ map switchVertexes (last plate))
  (concat $ map (\x -> [x !! 6, x !! 5]) $ reverse $ map switchVertexes (last plate))

plateRightWall :: Plate -> Wall V.Vec
plateRightWall plate = Wall
  (concat $ map (\x -> [x !! 3, x !! 2]) $ map switchVertexes (map last plate))
  (concat $ map (\x -> [x !! 7, x !! 6]) $ map switchVertexes (map last plate))

plateLeftWall :: Plate -> Wall V.Vec
plateLeftWall plate = Wall
  (concat $ map (\x -> [x !! 1, x !! 0]) $ reverse $ map switchVertexes (map head plate))
  (concat $ map (\x -> [x !! 5, x !! 4]) $ reverse $ map switchVertexes (map head plate))

platePerimeter :: Plate -> Sandwidge V.Vec
platePerimeter p =
  let (Wall t b) = plateFrontWall p <> plateRightWall p <>
                   plateBackWall p <> plateLeftWall p
  in Sandwidge t b
