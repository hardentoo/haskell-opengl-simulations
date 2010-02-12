{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, TypeFamilies #-}

module Data.GL.Matrix (
    Matrix(..), translateM, rotateM, translate, rotate
) where
import Graphics.UI.GLUT hiding (translate,rotate)
import qualified Graphics.UI.GLUT as GL

import Data.GL.Vector
import Data.Vec (Fold,Mat44,matToList,matFromList,matFromLists,Vec3)
import Data.Vec.Base ((:.)(..))
import Data.Vec.Packed
import Data.Vec.LinAlg.Transform3D

translateM :: MatrixComponent c => Vector3 c -> IO ()
translateM = GL.translate

rotateM :: MatrixComponent c => c -> Vector3 c -> IO ()
rotateM = GL.rotate

toGLmat :: (Fold v c, Fold m v, MatrixComponent c) => m -> IO (GLmatrix c)
toGLmat = newMatrix RowMajor . matToList

identity :: Mat44 GLdouble
identity = matFromLists [ take 4 $ drop i $ cycle [1,0,0,0] | i <- [4,3..1] ]

translate :: Real a => Vector3 a -> Mat44 GLdouble
translate (Vector3 x y z) = matFromList $ map g $ matToList
    $ translation (unpack $ Vec3D (f x) (f y) (f z))
    where
        f = fromRational . toRational
        g = fromRational . toRational

rotate = undefined

type Vec3d = Packed (Vec3 GLdouble)

instance PackedVec (Vec3 GLdouble) where 
    data Packed (Vec3 GLdouble) = Vec3d
        {-#UNPACK#-} !GLdouble
        {-#UNPACK#-} !GLdouble
        {-#UNPACK#-} !GLdouble 
    pack (a:.b:.c:.()) = Vec3d a b c
    unpack (Vec3d a b c) = a:.b:.c:.()
    {-# INLINE pack #-}
    {-# INLINE unpack #-}

instance Num (Mat44 GLdouble) where
    fromInteger x = scaling (unpack $ Vec3d y y y)
        where y = fromRational $ toRational x
