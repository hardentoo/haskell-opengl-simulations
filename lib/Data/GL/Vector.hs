{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GL.Vector (
    Vector(..), dot,
    vector3f, vector3d, vector4f, vector4d,
    vertex3f, vertex3d, vertex4f, vertex4d,
    color3f, color3d, color4f, color4d,
    texCoord1f, texCoord1d, texCoord2f, texCoord2d,
    texCoord3f, texCoord3d, texCoord4f, texCoord4d
) where
import Graphics.UI.GLUT

class Real a => Vector t a where
    vectorList :: t a -> [a]
    listVector :: [a] -> t a
    
    fromVector :: Vector u a => t a -> u a
    fromVector = listVector . vectorList
    
    (<.>) :: t a -> t a -> a -- dot product
    v1 <.> v2 = sum $ zipWith (*) (vectorList v1) (vectorList v2)
    (<^>) :: t a -> t a -> t a -- cross product

-- alias for <.>
dot :: (Vector t a) => t a -> t a -> a
dot = (<.>)

-- Real instances, vectors
instance Real a => Vector Vector1 a where
    vectorList (Vector1 x) = [x]
    listVector (x:_) = Vector1 x
    _ <^> _ = error "Cross product is not defined in 1 dimension (only 3 or 7)"

instance Real a => Vector Vector2 a where
    vectorList (Vector2 x y) = [x,y]
    listVector (x:y:_) = Vector2 x y
    _ <^> _ = error "Cross product is not defined in 2 dimensions (only 3 or 7)"

instance Real a => Vector Vector3 a where
    vectorList (Vector3 x y z) = [x,y,z]
    listVector (x:y:z:_) = Vector3 x y z
    (Vector3 x1 y1 z1) <^> (Vector3 x2 y2 z2) = Vector3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
    
instance Real a => Vector Vector4 a where
    vectorList (Vector4 x y z w) = [x,y,z,w]
    listVector (x:y:z:w:_) = Vector4 x y z w
    _ <^> _ = error "Cross product is not defined in 4 dimensions (only 3 or 7)"

-- Real instances, vertexes
instance Real a => Vector Vertex1 a where
    vectorList (Vertex1 x) = [x]
    listVector (x:_) = Vertex1 x
    _ <^> _ = error "Cross product is not defined in 1 dimension (only 3 or 7)"

instance Real a => Vector Vertex2 a where
    vectorList (Vertex2 x y) = [x,y]
    listVector (x:y:_) = Vertex2 x y
    (Vertex2 x1 y1) <.> (Vertex2 x2 y2) = (x1 * x2) + (y1 + y2)
    _ <^> _ = error "Cross product is not defined in 2 dimensions (only 3 or 7)"

instance Real a => Vector Vertex3 a where
    vectorList (Vertex3 x y z) = [x,y,z]
    listVector (x:y:z:_) = Vertex3 x y z
    (Vertex3 x1 y1 z1) <^> (Vertex3 x2 y2 z2) = Vertex3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
    
instance Real a => Vector Vertex4 a where
    vectorList (Vertex4 x y z w) = [x,y,z,w]
    listVector (x:y:z:w:_) = Vertex4 x y z w
    _ <^> _ = error "Cross product is not defined in 4 dimensions (only 3 or 7)"

-- Real instances, colors
instance Real a => Vector Color3 a where
    vectorList (Color3 x y z) = [x,y,z]
    listVector (x:y:z:_) = Color3 x y z
    (Color3 x1 y1 z1) <^> (Color3 x2 y2 z2) = Color3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
 
instance Real a => Vector Color4 a where
    vectorList (Color4 x y z w) = [x,y,z,w]
    listVector (x:y:z:w:_) = Color4 x y z w
    _ <^> _ = error "Cross product is not defined in 4 dimensions (only 3 or 7)"

-- Real instances, texture coordinates
instance Real a => Vector TexCoord1 a where
    vectorList (TexCoord1 x) = [x]
    listVector (x:_) = TexCoord1 x
    _ <^> _ = error "Cross product is not defined in 1 dimension (only 3 or 7)"

instance Real a => Vector TexCoord2 a where
    vectorList (TexCoord2 x y) = [x,y]
    listVector (x:y:_) = TexCoord2 x y
    _ <^> _ = error "Cross product is not defined in 2 dimensions (only 3 or 7)"

instance Real a => Vector TexCoord3 a where
    vectorList (TexCoord3 x y z) = [x,y,z]
    listVector (x:y:z:_) = TexCoord3 x y z
    (TexCoord3 x1 y1 z1) <^> (TexCoord3 x2 y2 z2) = TexCoord3 x y z
        where
            x = (y1 * z2 - y1 * z2)
            y = (z1 * x2 - x1 * z2)
            z = (x1 * y2 - y1 * x2)
    
instance Real a => Vector TexCoord4 a where
    _ <^> _ = error "Cross product is not defined in 4 dimensions (only 3 or 7)"
    vectorList (TexCoord4 x y z w) = [x,y,z,w]
    listVector (x:y:z:w:_) = TexCoord4 x y z w

-- generic Num instances
instance (Num a, Vector t a, Show (t a), Eq (t a)) => Num (t a) where
    v1 + v2 = listVector $ zipWith (+) (vectorList v1) (vectorList v2)
    v1 * v2 = listVector $ zipWith (*) (vectorList v1) (vectorList v2)
    v1 - v2 = listVector $ zipWith (-) (vectorList v1) (vectorList v2)
    negate = listVector . map negate . vectorList
    abs = listVector . map abs . vectorList
    signum = listVector . map signum . vectorList
    fromInteger x = listVector $ map fromInteger $ repeat x

instance (Fractional a, Show (t a), Eq (t a), Vector t a)
    => Fractional (t a) where
    v1 / v2 = listVector $ zipWith (/) (vectorList v1) (vectorList v2)
    recip = listVector . map recip . vectorList
    fromRational = listVector . repeat . fromRational

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
