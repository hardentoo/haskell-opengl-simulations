module Data.GL.IO where
import Graphics.UI.GLUT
import Data.GL.Vector

vertex3fM :: Real a => a -> a -> a -> IO ()
vertex3fM x y z = vertex $ vertex3f x y z

vertex3dM :: Real a => a -> a -> a -> IO ()
vertex3dM x y z = vertex $ vertex3f x y z

color3fM :: Real a => a -> a -> a -> IO ()
color3fM x y z = color $ color3f x y z

color3dM :: Real a => a -> a -> a -> IO ()
color3dM x y z = color $ color3d x y z
