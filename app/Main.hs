module Main where

import Lib
import System.IO
import Data.Glome.Vec
import Data.List
import Control.Monad.State
import Data.Foldable

test = xfm_point (rotate (vec 1 0 0) (deg 90)) (vec 0 1 0)

-- We assume next view location:
-- + x goes from left to right
-- + y goes from "near" to "far"
data OldSocket = OldSocket Int deriving Show

data Switch = Switch {
  pos :: Vec,
  angles :: Vec
  } deriving Show;

type PolyhedronSurface = [Int]

data PolyhedronSocket = PolyhedronSocket {
  socketVertIds :: [Int]
  } deriving Show

type PolyhedronMonad a = State (Int, [Vec], [PolyhedronSurface]) a

-- get Socket Vertex
sv :: Int -> PolyhedronSocket -> Int
sv n (PolyhedronSocket xs) = xs !! n

addVertex :: Vec -> PolyhedronMonad Int
addVertex v = do (idx, vert', surf) <- get
                 put (idx + 1, vert' ++ [v], surf)
                 return idx

addSurface :: PolyhedronSurface -> PolyhedronMonad ()
addSurface surf = do (idx, vert', sx) <- get
                     put (idx, vert', sx ++ [surf])
                     return ()

addSurfaceSandwich :: PolyhedronSurface -> PolyhedronMonad ()
addSurfaceSandwich v = do addSurface v
                          addSurface (reverse $ map topToBottom v)

addSwitch :: Switch -> PolyhedronMonad PolyhedronSocket
addSwitch (Switch centerPos (Vec ax ay az)) = do
  ids <- mapM addVertex [
      move $ vec (-w) (-w) h
    , move $ vec (-w) w    h
    , move $ vec w    w    h
    , move $ vec w    (-w) h
    , move $ vec (-w) (-w) (-h)
    , move $ vec (-w) w    (-h)
    , move $ vec w    w    (-h)
    , move $ vec w    (-w) (-h)
    ]
  return (PolyhedronSocket ids)
  where
    rotation = compose [rotate (vec 1 0 0) (deg ax)
                       , rotate (vec 0 1 0) (deg ay)
                       , rotate (vec 0 0 1) (deg az)]
    move = xfm_point (xfm_mult (translate centerPos) rotation)
    w  = switchWidth / 2
    h  = switchHeight / 2

addTopBottomSurf :: PolyhedronSocket -> PolyhedronMonad ()
addTopBottomSurf sw =
  addSurfaceSandwich $ map (socketVertIds sw !!) [0, 1, 2, 3]

addLeftToRightSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addLeftToRightSurf l r =
  addSurfaceSandwich [sv 1 r, sv 0 r, sv 3 l, sv 2 l]

addNearToFarSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addNearToFarSurf n f =
  addSurfaceSandwich [sv 1 n, sv 0 f, sv 3 f, sv 2 n]

addMiddleSurf :: PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad ()
addMiddleSurf s0 s1 s2 s3 =
  addSurfaceSandwich [sv 2 s0, sv 3 s1, sv 0 s2, sv 1 s3]

-- right_wall sw = [verts sw [3, 2, 6, 7]]

-- left_wall sw = [verts sw [4, 5, 1, 0]]

-- near_wall sw = [verts sw [0, 3, 7, 4]]

-- far_wall sw = [verts sw [2, 1, 5, 6]]

-- connect_right_wall_near_far n f = [[t2 n, t3 f, b3 f, b2 n]]

-- connect_left_wall_near_far n f = [[t0 f, t1 n, b1 n, b0 f]]

-- connect_near_wall_left_right l r = [[t3 l, t0 r, b0 r, b3 l]]

-- connect_far_wall_left_right l r = [[t1 r, t2 l, b2 l, b1 r]]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = drop 2 $ scanl (\(_, b) c -> (b, c)) (head xs, head xs) xs

eachSquare :: [[PolyhedronSocket]]
           -> (PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronSocket -> PolyhedronMonad a)
           -> PolyhedronMonad ()
eachSquare sockets f = do
  for_ [0 .. (height - 2)] $ \y' ->
    for_ [0 .. (width - 2)] $ \x' -> do
        f (sockets !! y' !! x') (sockets !! (y' + 1) !! x') (sockets !! (y' + 1) !! (x' + 1)) (sockets !! y' !! (x' + 1))
  where
    width = length (head sockets)
    height = length sockets

addFrontWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addFrontWall sockets = do
  let frontSurfTopLine = concat $ map (\s -> [sv 0 s, sv 3 s]) $ head sockets
  addSurface(frontSurfTopLine ++ reverse (map topToBottom frontSurfTopLine))

addBackWall :: [[PolyhedronSocket]] -> PolyhedronMonad ()
addBackWall sockets = do
  let backSurfTopLine = concat $ map (\s -> [sv 1 s, sv 2 s]) $ last sockets
  addSurface(reverse backSurfTopLine ++ map topToBottom backSurfTopLine)

addLeftWall sockets = do
  let leftSurfTopLine = concat $ map (\s -> [sv 0 s, sv 1 s]) $ map head sockets
  addSurface(reverse leftSurfTopLine ++ map topToBottom leftSurfTopLine)

addRightWall sockets = do
  let rightSurfTopLine = concat $ map (\s -> [sv 3 s, sv 2 s]) $ map last sockets
  addSurface(rightSurfTopLine ++ reverse (map topToBottom rightSurfTopLine))

buildPlate :: [[Switch]] -> PolyhedronMonad ()
buildPlate switches = do
  sockets <- sequence $ map (sequence . map addSwitch) (switches)
  for_ (concat sockets) addTopBottomSurf
  for_ sockets $ \line ->
    for_ (pairs $ line) (uncurry addLeftToRightSurf)
  for_ (transpose sockets) $ \row ->
    for_ (pairs $ row) (uncurry addNearToFarSurf)
  eachSquare sockets addMiddleSurf
  addFrontWall sockets
  addBackWall sockets
  addLeftWall sockets
  addRightWall sockets
  return ()

renderPolyhedron :: PolyhedronMonad [String]
renderPolyhedron = do (_, verts', surfs) <- get
                      return [
                        "polyhedron("
                        , "points=["
                        , intercalate ",\n" (fmap renderVec verts')
                        , "],"
                        , "faces=["
                        , intercalate ",\n" (fmap renderSurface surfs)
                        , "]);"
                        ]

switchWidth = 19.05
switchHeight = 5
holeWidth = 14
holeHeight = 12
keycapBottomWidth = 19.05
keycapTopWidth = 14
keycapHeight = 10
keycapElevation = 6

renderVec :: Vec -> String
renderVec (Vec x y z) = "[" ++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ "]"

renderSurface :: PolyhedronSurface -> String
renderSurface xs = show xs

topToBottom = (+ 4)
sandwich xs = [xs, reverse $ map topToBottom xs]

renderHoleCube (Switch (Vec x y z) (Vec ax ay az)) = [
  "translate(" ++ show [x, y, z] ++ ")"
  , "rotate(" ++ show [ax, ay, az] ++ ")"
  , "translate(" ++ show [-hw, -hw, -hh] ++ ")"
  , "cube(" ++ show [holeWidth, holeWidth, holeHeight] ++ ");"
  ]
  where
    hw = holeWidth / 2
    hh = holeHeight / 2

renderKeycap (Switch (Vec x y z) (Vec ax ay az)) = [
  "translate(" ++ show [x, y, z] ++ ")"
  , "rotate(" ++ show [ax, ay, az] ++ ")"
  , "hull() {"
  , "  translate(" ++ show [-hbw, -hbw, keycapElevation] ++ ")"
  , "    cube(" ++ show [keycapBottomWidth, keycapBottomWidth, 1] ++ ");"
  , "  translate(" ++ show [-htw, -htw, (keycapElevation + keycapHeight)] ++ ")"
  , "    cube(" ++ show [keycapTopWidth, keycapTopWidth, 1] ++ ");"
  , "};"
  ]
  where
    hbw = keycapBottomWidth / 2
    htw = keycapTopWidth / 2

renderHoles switches = concat (map renderHoleCube switches)

renderKeycaps switches = concat (map renderKeycap switches)

renderDifference xs ys =
  "difference() {" : xs ++ ys ++ ["};"]

renderUnion xs ys =
  "union() {" : xs ++ ys ++ ["};"]

color c xs = "color(" : [show c] ++ [") {"] ++ xs ++ ["}"]

render :: [[Switch]] -> [String]
render switches = renderUnion plate (renderKeycaps $ concat switches)
  where
    body = buildPlate switches >> renderPolyhedron
    platePolyhedron = evalState body (0, [], [])
    plate = renderDifference platePolyhedron (renderHoles $ concat switches)

mainPlate :: [[Switch]]
mainPlate = [
  [ Switch (vec 0  (0) 7) (vec (-25) 10 5)
  , Switch (vec 22 (2) 4) (vec (-25) 0 5)
  , Switch (vec 44 (2) 4) (vec (-25) 0 0)
  , Switch (vec 66 (4) 4) (vec (-25) 0 0)
  , Switch (vec 90 (-2) 6) (vec (-25) (-10) (-15))
  , Switch (vec 111 (-6) 12) (vec (-25) (-20) (-15))
  ], [ Switch (vec 0  23 2) (vec (0) 10 5) , Switch (vec 22 25 0) (vec (0) 0 5)
  , Switch (vec 44 28 0) (vec (0) 0 0)
  , Switch (vec 66 30 0) (vec (0) 0 0)
  , Switch (vec 90 22 2) (vec (0) (-10) (-15))
  , Switch (vec 111 18 09) (vec (0) (-20) (-15))
  ],
  [ Switch (vec 0  46 8) (vec (25) 10 5)
  , Switch (vec 22 48 6) (vec (25) 0 5)
  , Switch (vec 44 54 6) (vec (25) 0 0)
  , Switch (vec 66 56 6) (vec (25) 0 0)
  , Switch (vec 90 46 8) (vec (25) (-10) (-15))
  , Switch (vec 111 42 15) (vec (30) (-23) (-15))
  ]
  ]

thumbPlate = [
  [ Switch (vec 0  0 6) (vec (-15) (15) 0)
  , Switch (vec 22 0 3) (vec (-15) 0 0)
  , Switch (vec 44 0 6) (vec (-15) (-15) 0)
  ],
  [ Switch (vec 0  22 3) (vec 0 (15) 0)
  , Switch (vec 22 22 0) (vec 0 0 0)
  , Switch (vec 44 22 3) (vec 0 (-15) 0)
  ],
  [ Switch (vec 0  44 6) (vec (15) (15) 0)
  , Switch (vec 22 44 3) (vec (15) 0 0)
  , Switch (vec 44 44 6) (vec (15) (-15) 0)
  ]
  ]

withKeycaps = False

main :: IO ()
main = do withFile "main_plate.scad" WriteMode $ \h -> mapM_ (hPutStrLn h) (render mainPlate)
