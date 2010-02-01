{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.GL.Matrix (
    Matrix(..)
) where
import Graphics.UI.GLUT hiding (Matrix,newMatrix)
import Data.GL.Vector
import System.IO.Unsafe (unsafePerformIO)

class Matrix t a where
    (<>) :: t a -> t a -> t a -- multiplication
    newMatrix :: IO () -> IO (t a)
    mRotate :: MatrixComponent b => b -> Vector3 b -> t a -> t a
    mTranslate :: MatrixComponent b => Vector3 b -> t a -> t a

instance MatrixComponent a => Matrix GLmatrix a where
    m1 <> m2 = unsafePerformIO $ newMatrix $ do
        multMatrix m1
        multMatrix m2
    
    newMatrix matM = preservingMatrix $ do
        loadIdentity
        matM
        get $ matrix Nothing
    
    mTranslate vec mat = unsafePerformIO $ preservingMatrix $ do
        loadIdentity
        multMatrix mat
        translate vec
        get $ matrix Nothing
    
    mRotate theta vec mat = unsafePerformIO $ preservingMatrix $ do
        loadIdentity
        multMatrix mat
        rotate theta vec
        get $ matrix Nothing
