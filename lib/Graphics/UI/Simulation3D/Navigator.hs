{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Simulation3D.Navigator (
    wasd, WASD(..)
) where

import Graphics.UI.Simulation3D.Base
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Transform
import Graphics.UI.GLUT hiding (Matrix,rotate,translate)
import qualified Data.Set as Set
import Control.Arrow (first,second,(&&&))

data WASD = WASD {
    rSpeed, tSpeed :: Double
}

wasd :: Simulation a => WASD -> HookIO a ()
wasd params = do
    cam <- getCamera
    inputState <- getInputState
    setCamera $ wasd' params cam inputState

wasd' :: WASD -> Camera -> InputState -> Camera
wasd' params cam inputState = cam' where
    cam' = if (SpecialKey KeyF5) `elem` keys
        then cam { cameraMatrix = ident 4 }
        else cam { cameraMatrix = mat' }
    
    mat = cameraMatrix cam
    mat' = case keys of
        [] -> mat
        _ -> (foldl1 (<>) $ map rKey keys) <> tMat <> mat
    tMat = translation (sum $ map tKey keys)
    
    keys = Set.elems $ inputKeySet inputState
    mpos = inputMousePos inputState
    prevPos = inputPrevMousePos inputState
    
    dt = tSpeed params
    drx = -(rSpeed params) * (fromIntegral $ fst mpos - fst prevPos)
    dry = -(rSpeed params) * (fromIntegral $ snd mpos - snd prevPos)
    
    tKey :: Key -> Vector Double
    tKey (Char 'w') = 3 |> [0,0,dt] -- forward
    tKey (Char 's') = 3 |> [0,0,-dt] -- back
    tKey (Char 'a') = 3 |> [dt,0,0] -- strafe left
    tKey (Char 'd') = 3 |> [-dt,0,0] -- strafe right
    tKey (Char 'q') = 3 |> [0,-dt,0] -- up
    tKey (Char 'z') = 3 |> [0,dt,0] -- down
    tKey _ = 3 |> [0,0,0]
    
    rKey :: Key -> Matrix Double
    rKey (MouseButton LeftButton) = r1 <> r2
        where
            r1 = rotation (AxisX dry)
            r2 = rotation (AxisY drx)
    rKey (MouseButton RightButton) = rotation (AxisZ drx)
    rKey _ = ident 4
