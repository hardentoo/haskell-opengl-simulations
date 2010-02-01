module Data.Matrix.GL where
import Graphics.UI.GLUT

--(<.>) ::
--v1 <.> v2 = 

vec3f :: Real a => a -> a -> a -> Vector3 GLfloat
vec3f x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational

vec3d :: Real a => a -> a -> a -> Vector3 GLdouble
vec3d x y z = Vector3 (f x) (f y) (f z)
    where f = fromRational . toRational
