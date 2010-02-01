{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Matrix.GL where
import Graphics.UI.GLUT

class Num a => Vector t a where
    (<.>) :: t a -> t a -> a -- dot product
    v1 <.> v2 = sum $ zipWith (*) (fromVector v1) (fromVector v2)
    
    fromVector :: t a -> [a]
    toVector :: [a] -> t a

-- alias for <.>
dot :: (Vector t a) => t a -> t a -> a
dot = (<.>)

instance Num a => Vector Vector3 a where
    (Vector3 x1 y1 z1) <.> (Vector3 x2 y2 z2) = 
        (x1 * x2) + (y1 * y2) + (z1 * z2)
    fromVector (Vector3 x y z) = [x,y,z]
    toVector (x:y:z:_) = Vector3 x y z

instance Num a => Vector Vertex3 a where
    (Vertex3 x1 y1 z1) <.> (Vertex3 x2 y2 z2) = 
        (x1 * x2) + (y1 * y2) + (z1 * z2)
    fromVector (Vertex3 x y z) = [x,y,z]
    toVector (x:y:z:_) = Vertex3 x y z

vec3f :: Real a => a -> a -> a -> Vector3 GLfloat
vec3f x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

vec3d :: Real a => a -> a -> a -> Vector3 GLdouble
vec3d x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

ver3f :: Real a => a -> a -> a -> IO ()
ver3f x y z = vertex $ Vertex3 (f x) (f y) (f z)
    where f = fromRational . toRational :: Real a => a -> GLfloat

ver3d :: Real a => a -> a -> a -> IO ()
ver3d x y z = vertex $ Vertex3 (f x) (f y) (f z)
    where f = fromRational . toRational :: Real a => a -> GLdouble

buildMatrix :: IO () -> IO (GLmatrix GLdouble)
buildMatrix matM = preservingMatrix $ do
    loadIdentity
    matM
    get $ matrix Nothing
