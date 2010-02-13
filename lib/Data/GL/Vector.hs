{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GL.Vector (
    vector1f, vector1d, vector2f, vector2d,
    vector3f, vector3d, vector4f, vector4d,
    vertex1f, vertex1d, vertex2f, vertex2d,
    vertex3f, vertex3d, vertex4f, vertex4d,
    color3f, color3d, color4f, color4d,
    texCoord1f, texCoord1d, texCoord2f, texCoord2d,
    texCoord3f, texCoord3d, texCoord4f, texCoord4d
) where

import qualified Graphics.UI.GLUT as GL
import Graphics.UI.GLUT hiding (Matrix)
import Numeric.LinearAlgebra

class Vectorize t a where
    toVec :: t a -> Vector a
    fromVec :: Vector a -> t a

-- vectorized vectors
instance MatrixComponent c => Vectorize Vector1 c where
    toVec (Vector1 x) = 1 |> [x]
    fromVec v = Vector1 x
        where [x] = toList v

instance MatrixComponent c => Vectorize Vector2 c where
    toVec (Vector2 x y) = 2 |> [x,y]
    fromVec v = Vector2 x y
        where [x,y] = toList v

instance MatrixComponent c => Vectorize Vector3 c where
    toVec (Vector3 x y z) = 3 |> [x,y,z]
    fromVec v = Vector3 x y z
        where [x,y,z] = toList v

instance MatrixComponent c => Vectorize Vector4 c where
    toVec (Vector4 x y z w) = 4 |> [x,y,z,w]
    fromVec v = Vector4 x y z w
        where [x,y,z,w] = toList v

-- vectorized vertexes
instance MatrixComponent c => Vectorize Vertex1 c where
    toVec (Vertex1 x) = 1 |> [x]
    fromVec v = Vertex1 x
        where [x] = toList v

instance MatrixComponent c => Vectorize Vertex2 c where
    toVec (Vertex2 x y) = 2 |> [x,y]
    fromVec v = Vertex2 x y
        where [x,y] = toList v

instance MatrixComponent c => Vectorize Vertex3 c where
    toVec (Vertex3 x y z) = 3 |> [x,y,z]
    fromVec v = Vertex3 x y z
        where [x,y,z] = toList v

instance MatrixComponent c => Vectorize Vertex4 c where
    toVec (Vertex4 x y z w) = 4 |> [x,y,z,w]
    fromVec v = Vertex4 x y z w
        where [x,y,z,w] = toList v

-- vectorized colors
instance MatrixComponent c => Vectorize Color3 c where
    toVec (Color3 x y z) = 3 |> [x,y,z]
    fromVec v = Color3 x y z
        where [x,y,z] = toList v

instance MatrixComponent c => Vectorize Color4 c where
    toVec (Color4 x y z w) = 4 |> [x,y,z,w]
    fromVec v = Color4 x y z w
        where [x,y,z,w] = toList v

-- vectorized texcoords
instance MatrixComponent c => Vectorize TexCoord1 c where
    toVec (TexCoord1 x) = 1 |> [x]
    fromVec v = TexCoord1 x
        where [x] = toList v

instance MatrixComponent c => Vectorize TexCoord2 c where
    toVec (TexCoord2 x y) = 2 |> [x,y]
    fromVec v = TexCoord2 x y
        where [x,y] = toList v

instance MatrixComponent c => Vectorize TexCoord3 c where
    toVec (TexCoord3 x y z) = 3 |> [x,y,z]
    fromVec v = TexCoord3 x y z
        where [x,y,z] = toList v

instance MatrixComponent c => Vectorize TexCoord4 c where
    toVec (TexCoord4 x y z w) = 4 |> [x,y,z,w]
    fromVec v = TexCoord4 x y z w
        where [x,y,z,w] = toList v

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
