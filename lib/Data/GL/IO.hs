module Data.GL.IO where
import Graphics.UI.GLUT
import Data.GL.Vector

vertex3fM :: Real a => a -> a -> a -> IO ()
vertex3fM x y z = vertex $ vertex3f x y z

vertex3dM :: Real a => a -> a -> a -> IO ()
vertex3dM x y z = vertex $ vertex3f x y z

vertex4fM :: Real a => a -> a -> a -> a -> IO ()
vertex4fM x y z w = vertex $ vertex4f x y z w

vertex4dM :: Real a => a -> a -> a -> a -> IO ()
vertex4dM x y z w = vertex $ vertex4f x y z w

color3fM :: Real a => a -> a -> a -> IO ()
color3fM x y z = color $ color3f x y z

color3dM :: Real a => a -> a -> a -> IO ()
color3dM x y z = color $ color3d x y z

color4fM :: Real a => a -> a -> a -> a -> IO ()
color4fM x y z w = color $ color4f x y z w

color4dM :: Real a => a -> a -> a -> a -> IO ()
color4dM x y z w = color $ color4d x y z w

texCoord1fM :: Real a => a -> IO ()
texCoord1fM x = texCoord $ texCoord1f x

texCoord1dM :: Real a => a -> IO ()
texCoord1dM x = texCoord $ texCoord1d x

texCoord2fM :: Real a => a -> a -> IO ()
texCoord2fM x y = texCoord $ texCoord2f x y

texCoord2dM :: Real a => a -> a -> IO ()
texCoord2dM x y = texCoord $ texCoord2d x y

texCoord3fM :: Real a => a -> a -> a -> IO ()
texCoord3fM x y z = texCoord $ texCoord3f x y z

texCoord3dM :: Real a => a -> a -> a -> IO ()
texCoord3dM x y z = texCoord $ texCoord3d x y z

texCoord4fM :: Real a => a -> a -> a -> a -> IO ()
texCoord4fM x y z w = texCoord $ texCoord4f x y z w

texCoord4dM :: Real a => a -> a -> a -> a -> IO ()
texCoord4dM x y z w = texCoord $ texCoord4d x y z w
