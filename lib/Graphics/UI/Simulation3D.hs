module Graphics.UI.Simulation3D (
    module Graphics.UI.GLUT,
    module Graphics.UI.Simulation3D.Base,
    module Graphics.UI.Simulation3D.Util,
    module Graphics.UI.Simulation3D.Navigator,
    module Graphics.UI.Simulation3D.Shader,
    module Numeric.LinearAlgebra,
    module Numeric.LinearAlgebra.Transform,
    defaultState, runSimulation,
) where

import Graphics.UI.GLUT hiding (Matrix(..),newMatrix,rotate,translate)

import Graphics.UI.Simulation3D.Base
import Graphics.UI.Simulation3D.Util
import Graphics.UI.Simulation3D.Shader
import Graphics.UI.Simulation3D.Navigator

import Numeric.LinearAlgebra.Transform
import Numeric.LinearAlgebra hiding (scale)
import qualified Data.Set as Set

defaultState :: Simulation a => a -> SimState a
defaultState sim = SimState {
    simulation = sim,
    simCamera = Camera {
        cameraFOV = 60,
        cameraNear = 0.1,
        cameraFar = 100000,
        cameraMatrix = rotate (AxisX $ - pi / 2)
            $ translation (3 |> [0,-0.5,-3])
    },
    simWindow = SimWindow {
        simWinTitle = "Simulation",
        simWinSize = (512,512),
        simWinPos = (0,0),
        simWinBG = color4cf 0.3 0.3 0.3 1.0
    },
    simModes = [ DoubleBuffered, RGBMode, WithDepthBuffer ],
    simInputState = InputState {
        inputKeySet = Set.empty,
        inputMousePos = (0,0),
        inputPrevMousePos = (0,0)
    },
    simFPS = 0.0
}

runSimulation :: Simulation a => a -> IO ()
runSimulation = runSimulationState . defaultState 
