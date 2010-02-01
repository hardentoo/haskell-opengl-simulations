-- navigation and spheres
module Main where
import Graphics.UI.GLUT
import Graphics.UI.GL.Simulation

data SphereSim = SphereSim

instance Simulation SphereSim where
    display sim = do
        renderObject Solid $ Sphere' 1 6 6
        return sim

main :: IO ()
main = runSimulation SphereSim
