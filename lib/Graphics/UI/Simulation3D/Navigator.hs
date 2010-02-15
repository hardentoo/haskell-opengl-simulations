{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Simulation3D.Navigator (
    wasdNav, WASD(..)
) where

import Graphics.UI.Simulation3D.Base
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Transform
import Graphics.UI.GLUT hiding (Matrix,rotate,translate)
import qualified Data.Set as Set

data WASD = WASD {
    rSpeed, tSpeed :: Double
}

wasdNav :: Simulation a => WASD -> HookIO a ()
wasdNav params = do
    cam <- getCamera
    inputState <- getInputState
    setCamera $ wasdNav' params cam inputState

wasdNav' :: WASD -> Camera -> InputState -> Camera
wasdNav' params cam inputState = cam' where
    cam' = case keys of
        [] -> cam
        _ -> cam { cameraMatrix = (cameraMatrix cam) <> rMat <> tMat }
    
    rMat, tMat :: Matrix Double
    rMat = foldl1 (+) $ map rKey keys
    tMat = translation (sum $ map tKey keys)
    
    keys = Set.elems $ inputKeySet inputState
    pos = inputMousePos inputState
    prevPos = inputPrevMousePos inputState
    
    dt = tSpeed params
    drx = (rSpeed params) * (fromIntegral $ fst pos - fst prevPos)
    dry = -(rSpeed params) * (fromIntegral $ snd pos - snd prevPos)
    
    tKey :: Key -> Vector Double
    tKey (Char 'w') = 3 |> [0,0,dt] -- forward
    tKey (Char 's') = 3 |> [0,0,-dt] -- back
    tKey (Char 'a') = 3 |> [dt,0,0] -- strafe left
    tKey (Char 'd') = 3 |> [-dt,0,0] -- strafe right
    tKey (Char 'q') = 3 |> [0,-dt,0] -- up
    tKey (Char 'z') = 3 |> [0,dt,0] -- down
    tKey _ = 3 |> [0,0,0]
    
    rKey :: Key -> Matrix Double
    rKey (MouseButton LeftButton) =
        rotate (AxisX dry) $ rotation (AxisY (-drx))
    rKey (MouseButton RightButton) = rotation (AxisZ drx)
    rKey _ = ident 4
