-- navigation and spheres
module Main where
import Graphics.UI.GLUT
import Graphics.UI.GL.Simulation
import Control.Monad (forM_)
import Data.Matrix.GL

data SphereSim = SphereSim

instance Simulation SphereSim where
    display sim = do
        drawFloor
        renderObject Solid $ Sphere' 1 6 6
        return sim

drawFloor :: IO ()
drawFloor = renderPrimitive Lines $ do
    forM_ [ -5 .. 5 ] $ \i -> do
        ver3f i (-5) 0 >> ver3f i 5 0 -- x-lines
        ver3f (-5) i 0 >> ver3f 5 i 0 -- y-lines

main :: IO ()
main = runSimulation SphereSim
