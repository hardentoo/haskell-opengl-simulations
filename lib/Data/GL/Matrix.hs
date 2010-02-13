{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Data.GL.Matrix (
    translateM, rotateM, toGLmat,
    module Numeric.LinearAlgebra,
    module Numeric.LinearAlgebra.Transform
) where
import Graphics.UI.GLUT hiding (Matrix(..),translate,rotate)
import qualified Graphics.UI.GLUT as GL

import Data.GL.Vector
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Transform

translateM :: MatrixComponent c => Vector3 c -> IO ()
translateM = GL.translate

rotateM :: MatrixComponent c => c -> Vector3 c -> IO ()
rotateM = GL.rotate

toGLmat :: (MatrixComponent e, Element e, Num (Matrix e), Linear Matrix e)
    => Matrix e -> IO (GLmatrix e)
toGLmat = GL.newMatrix RowMajor . concat . toLists
