{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Simulation3D.Navigator (
    wasd, WASD(..)
) where

import Graphics.UI.Simulation3D.Base
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Transform
import Graphics.UI.GLUT hiding (Matrix,rotate,translate)
import qualified Data.Set as Set
import Control.Arrow ((&&&))

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
        then cam { cameraPos = 4 |> [0,0,0,1], cameraRotation = ident 4 }
        else cam { cameraPos = pos', cameraRotation = rot' }
    (pos,rot) = cameraPos &&& cameraRotation $ cam
    
    pos' = pos + ((foldl (+) (4 |> [0,0,0,1]) $ map tKey keys) <> rot)
    rot' = foldl (<>) rot $ map rKey keys
    
    keys = Set.elems $ inputKeySet inputState
    mpos = inputMousePos inputState
    prevPos = inputPrevMousePos inputState
    
    dt = tSpeed params
    drx = (rSpeed params) * (fromIntegral $ fst mpos - fst prevPos)
    dry = -(rSpeed params) * (fromIntegral $ snd mpos - snd prevPos)
    
    tKey :: Key -> Vector Double
    tKey (Char 'w') = 4 |> [0,0,dt,1] -- forward
    tKey (Char 's') = 4 |> [0,0,-dt,1] -- back
    tKey (Char 'a') = 4 |> [dt,0,0,1] -- strafe left
    tKey (Char 'd') = 4 |> [-dt,0,0,1] -- strafe right
    tKey (Char 'q') = 4 |> [0,-dt,0,1] -- up
    tKey (Char 'z') = 4 |> [0,dt,0,1] -- down
    tKey _ = 4 |> [0,0,0,1]
    
    rKey :: Key -> Matrix Double
    rKey (MouseButton LeftButton) =
        rotate (AxisZ (-drx)) $ rotate (AxisY (-ay)) $ rotation (AxisX ax)
        where
            (ax,ay) = (dry*) . cos &&& (dry*) . sin $ atan2 x' y'
            (x':y':_) = toList pos
    rKey (MouseButton RightButton) = rotation (AxisAngle drx pos3)
        where pos3 = (3 |>) $ toList $ (4 |> [0,0,1,1]) <> rot
    rKey _ = ident 4
