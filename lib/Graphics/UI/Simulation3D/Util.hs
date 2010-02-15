{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.UI.Simulation3D.Util (
    elapsed, runAtFPS, runWithFPS, exitSimulation,
    translateM, rotateM, toGLmat,
    vertex3fM, vertex3dM, vertex4fM, vertex4dM,
    color3fM, color3dM, color4fM, color4dM,
    color3cfM, color3cdM, color4cfM, color4cdM,
    texCoord1fM, texCoord1dM, texCoord2fM, texCoord2dM,
    texCoord3fM, texCoord3dM, texCoord4fM, texCoord4dM,
    vector1f, vector1d, vector2f, vector2d,
    vector3f, vector3d, vector4f, vector4d,
    vertex1f, vertex1d, vertex2f, vertex2d,
    vertex3f, vertex3d, vertex4f, vertex4d,
    color3f, color3d, color4f, color4d,
    color3cf, color3cd, color4cf, color4cd,
    texCoord1f, texCoord1d, texCoord2f, texCoord2d,
    texCoord3f, texCoord3d, texCoord4f, texCoord4d
) where

import Graphics.UI.GLUT hiding (Matrix(..),translate,rotate)
import qualified Graphics.UI.GLUT as GL
import Numeric.LinearAlgebra

import Control.Monad (when,liftM)
import Data.Maybe (isJust,fromJust)
import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- elapsed time to run the supplied action in seconds
elapsed :: Floating a => IO b -> IO (a,b)
elapsed m = do
    t1 <- getCurrentTime
    result <- m
    t2 <- getCurrentTime
    let t = fromRational $ toRational $ diffUTCTime t1 t2
    return (t,result)

-- run an action at a number of frames per second, if possible
-- if not enough time has elapsed, waits until fps is achieved
runAtFPS :: (Floating a, RealFrac a) => a -> IO b -> IO b
runAtFPS fps m = do
    (t',result) <- elapsed m
    let t = recip fps - t'
    when (t > 0) $ threadDelay $ floor (t * 1e6)
    return result

-- run an action, returning the frames per second along with the result
runWithFPS :: (Floating a, RealFrac a) => IO b -> IO (a,b)
runWithFPS = (liftM $ first recip) . elapsed

exitSimulation :: IO ()
exitSimulation = do
    leaveMainLoop
    win <- get currentWindow
    when (isJust win) $ destroyWindow (fromJust win)

translateM :: MatrixComponent c => Vector3 c -> IO ()
translateM = GL.translate

rotateM :: MatrixComponent c => c -> Vector3 c -> IO ()
rotateM = GL.rotate

toGLmat :: (Real e, Num (Matrix e), Linear Matrix e)
    => Matrix e -> IO (GLmatrix GLdouble)
toGLmat = GL.newMatrix RowMajor
    . map (fromRational . toRational)
    . concat . toLists

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

color3cfM :: Real a => a -> a -> a -> IO ()
color3cfM x y z = color $ color3cf x y z

color3cdM :: Real a => a -> a -> a -> IO ()
color3cdM x y z = color $ color3cd x y z

color4cfM :: Real a => a -> a -> a -> a -> IO ()
color4cfM x y z w = color $ color4cf x y z w

color4cdM :: Real a => a -> a -> a -> a -> IO ()
color4cdM x y z w = color $ color4cd x y z w

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

-- litany of convenience functions for vector-esque operations
vector1f :: Real a => a -> Vector1 GLfloat
vector1f x = Vector1 (f x)
    where f = fromRational . toRational

vector1d :: Real a => a -> Vector1 GLdouble
vector1d x = Vector1 (f x)
    where f = fromRational . toRational

vector2f :: Real a => a -> a -> Vector2 GLfloat
vector2f x y = Vector2 (f x) (f y)
    where f = fromRational . toRational

vector2d :: Real a => a -> a -> Vector2 GLdouble
vector2d x y = Vector2 (f x) (f y)
    where f = fromRational . toRational

vector3f :: Real a => a -> a -> a -> Vector3 GLfloat
vector3f x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

vector3d :: Real a => a -> a -> a -> Vector3 GLdouble
vector3d x y z = Vector3 (d x) (d y) (d z)
    where d = fromRational . toRational

vector4f :: Real a => a -> a -> a -> a -> Vector4 GLfloat
vector4f x y z w = Vector4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

vector4d :: Real a => a -> a -> a -> a -> Vector4 GLdouble
vector4d x y z w = Vector4 (d x) (d y) (d z) (d w)
    where d = fromRational . toRational

vertex1f :: Real a => a -> a -> Vertex2 GLfloat
vertex1f x y = Vertex2 (f x) (f y)
    where f = fromRational . toRational

vertex1d :: Real a => a -> Vertex1 GLdouble
vertex1d x = Vertex1 (f x)
    where f = fromRational . toRational

vertex2f :: Real a => a -> a -> Vertex2 GLfloat
vertex2f x y = Vertex2 (f x) (f y)
    where f = fromRational . toRational

vertex2d :: Real a => a -> a -> Vertex2 GLdouble
vertex2d x y = Vertex2 (f x) (f y)
    where f = fromRational . toRational

vertex3f :: Real a => a -> a -> a -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (f x) (f y) (f z)
    where f = fromRational . toRational

vertex3d :: Real a => a -> a -> a -> Vertex3 GLdouble
vertex3d x y z = Vertex3 (d x) (d y) (d z)
    where d = fromRational . toRational

vertex4f :: Real a => a -> a -> a -> a -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

vertex4d :: Real a => a -> a -> a -> a -> Vertex4 GLdouble
vertex4d x y z w = Vertex4 (d x) (d y) (d z) (d w)
    where d = fromRational . toRational

color3f :: Real a => a -> a -> a -> Color3 GLfloat
color3f x y z = Color3 (f x) (f y) (f z)
    where f = fromRational . toRational

color3d :: Real a => a -> a -> a -> Color3 GLdouble
color3d x y z = Color3 (d x) (d y) (d z)
    where d = fromRational . toRational

color4f :: Real a => a -> a -> a -> a -> Color4 GLfloat
color4f x y z w = Color4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

color4d :: Real a => a -> a -> a -> a -> Color4 GLdouble
color4d x y z w = Color4 (d x) (d y) (d z) (d w)
    where d = fromRational . toRational

color3cf :: Real a => a -> a -> a -> Color3 GLclampf
color3cf x y z = Color3 (f x) (f y) (f z)
    where f = fromRational . toRational

color3cd :: Real a => a -> a -> a -> Color3 GLclampd
color3cd x y z = Color3 (d x) (d y) (d z)
    where d = fromRational . toRational

color4cf :: Real a => a -> a -> a -> a -> Color4 GLclampf
color4cf x y z w = Color4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

color4cd :: Real a => a -> a -> a -> a -> Color4 GLclampd
color4cd x y z w = Color4 (d x) (d y) (d z) (d w)
    where d = fromRational . toRational

texCoord1f :: Real a => a -> TexCoord1 GLfloat
texCoord1f x = TexCoord1 (f x)
    where f = fromRational . toRational

texCoord1d :: Real a => a -> TexCoord1 GLdouble
texCoord1d x = TexCoord1 (d x)
    where d = fromRational . toRational

texCoord2f :: Real a => a -> a -> TexCoord2 GLfloat
texCoord2f x y = TexCoord2 (f x) (f y)
    where f = fromRational . toRational

texCoord2d :: Real a => a -> a -> TexCoord2 GLdouble
texCoord2d x y = TexCoord2 (d x) (d y)
    where d = fromRational . toRational

texCoord3f :: Real a => a -> a -> a -> TexCoord3 GLfloat
texCoord3f x y z = TexCoord3 (f x) (f y) (f z)
    where f = fromRational . toRational

texCoord3d :: Real a => a -> a -> a -> TexCoord3 GLdouble
texCoord3d x y z = TexCoord3 (d x) (d y) (d z)
    where d = fromRational . toRational

texCoord4f :: Real a => a -> a -> a -> a -> TexCoord4 GLfloat
texCoord4f x y z w = TexCoord4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

texCoord4d :: Real a => a -> a -> a -> a -> TexCoord4 GLdouble
texCoord4d x y z w = TexCoord4 (d x) (d y) (d z) (d w)
    where d = fromRational . toRational
