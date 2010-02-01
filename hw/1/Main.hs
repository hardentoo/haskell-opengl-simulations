-- navigation and spheres
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import Graphics.UI.GLUT
import Graphics.UI.GL.Simulation
import Control.Monad (forM_)
import Data.Matrix.GL

data SphereSim = SphereSim {
    simTheta :: GLfloat
}

instance Simulation SphereSim where
    display sim = do
        let theta = simTheta sim
        
        color3fM 0.8 0.8 1 >> drawFloor
        
        preservingMatrix $ do
            color3fM 0 1 1
            rotate theta $ vector3f 0 0 1
            renderObject Solid $ Sphere' 1 6 6
        return sim { simTheta = theta + 0.1 }

drawFloor :: IO ()
drawFloor = renderPrimitive Lines $ do
    forM_ [ -5 .. 5 ] $ \i ->
        mapM_ vertex [
            vertex3f i (-5) 0, vertex3f i 5 0, -- x-lines
            vertex3f (-5) i 0, vertex3f 5 i 0 -- x-lines
        ]

main :: IO ()
main = runSimulation sim where
    sim = SphereSim { simTheta = 0.0 }
