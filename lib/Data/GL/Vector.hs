{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.GL.Vector (
    Vector(..), dot,
    vector3f, vector3d, vector4f, vector4d,
    vertex3f, vertex3d, vertex4f, vertex4d,
    color3f, color3d, color4f, color4d
) where
import Graphics.UI.GLUT

class Real a => Vector t a where
    (<.>) :: t a -> t a -> a -- dot product
    v1 <.> v2 = sum $ zipWith (*) (fromVector v1) (fromVector v2)
    
    (<^>) :: t a -> t a -> t a -- cross product
    
    (*>) :: a -> t a -> t a -- left scalar multiply
    f *> v = toVector $ map (*f) $ fromVector v
    (<*) :: t a -> a -> t a -- right scalar multiply
    (<*) = flip (*>)
    
    (+>) :: a -> t a -> t a -- left scalar addition
    f +> v = toVector $ map (+f) $ fromVector v
    (<+) :: t a -> a -> t a -- right scalar addition
    (<+) = flip (+>)
    
    (<+>) :: t a -> t a -> t a -- vector addition
    v1 <+> v2 = toVector $ zipWith (+) (fromVector v1) (fromVector v2)
    
    (<->) :: t a -> t a -> t a -- vector subtraction
    v1 <-> v2 = toVector $ zipWith (-) (fromVector v1) (fromVector v2)
    
    fromVector :: t a -> [a]
    toVector :: [a] -> t a
    
-- alias for <.>
dot :: (Vector t a) => t a -> t a -> a
dot = (<.>)

instance Real a => Vector Vector3 a where
    (Vector3 x1 y1 z1) <.> (Vector3 x2 y2 z2) = 
        (x1 * x2) + (y1 * y2) + (z1 * z2)
    
    (Vector3 x1 y1 z1) <^> (Vector3 x2 y2 z2) = Vector3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
    
    (Vector3 x1 y1 z1) <+> (Vector3 x2 y2 z2) = 
        Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
    
    fromVector (Vector3 x y z) = [x,y,z]
    toVector (x:y:z:_) = Vector3 x y z
    
instance Real a => Vector Vertex3 a where
    (Vertex3 x1 y1 z1) <.> (Vertex3 x2 y2 z2) = 
        (x1 * x2) + (y1 * y2) + (z1 * z2)
    
    (Vertex3 x1 y1 z1) <^> (Vertex3 x2 y2 z2) = Vertex3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
    
    (Vertex3 x1 y1 z1) <+> (Vertex3 x2 y2 z2) = 
        Vertex3 (x1 + x2) (y1 + y2) (z1 + z2)
    
    fromVector (Vertex3 x y z) = [x,y,z]
    toVector (x:y:z:_) = Vertex3 x y z

-- convenience functions for vector-esque operations

vector3f :: Real a => a -> a -> a -> Vector3 GLfloat
vector3f x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

vector3d :: Real a => a -> a -> a -> Vector3 GLdouble
vector3d x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

vector4f :: Real a => a -> a -> a -> a -> Vector4 GLfloat
vector4f x y z w = Vector4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

vector4d :: Real a => a -> a -> a -> a -> Vector4 GLdouble
vector4d x y z w = Vector4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

vertex3f :: Real a => a -> a -> a -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (f x) (f y) (f z)
    where f = fromRational . toRational

vertex3d :: Real a => a -> a -> a -> Vertex3 GLdouble
vertex3d x y z = Vertex3 (f x) (f y) (f z)
    where f = fromRational . toRational

vertex4f :: Real a => a -> a -> a -> a -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

vertex4d :: Real a => a -> a -> a -> a -> Vertex4 GLdouble
vertex4d x y z w = Vertex4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

color3f :: Real a => a -> a -> a -> Color3 GLfloat
color3f x y z = Color3 (f x) (f y) (f z)
    where f = fromRational . toRational

color3d :: Real a => a -> a -> a -> Color3 GLdouble
color3d x y z = Color3 (f x) (f y) (f z)
    where f = fromRational . toRational

color4f :: Real a => a -> a -> a -> a -> Color4 GLfloat
color4f x y z w = Color4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational

color4d :: Real a => a -> a -> a -> a -> Color4 GLdouble
color4d x y z w = Color4 (f x) (f y) (f z) (f w)
    where f = fromRational . toRational
