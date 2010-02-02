-- navigation and spheres
{-# LANGUAGE QuasiQuotes #-} -- for heredocs
module Main where
import Graphics.UI.GL.Simulation
import Control.GL.Shader (newProgram,withProgram,here)
import Control.Monad (forM_)
import Data.IORef (IORef,newIORef)

data SphereSim = SphereSim {
    simTheta :: GLfloat,
    simShader :: Program
}

instance Simulation SphereSim where
    display sim = runWithFPS 60 $ do
        color3fM 0.8 0.8 1 >> drawFloor
        
        let theta = simTheta sim
        withProgram (simShader sim) $ preservingMatrix $ do
            color3fM 0 1 1
            rotate theta $ vector3f 0 0 1
            renderObject Solid $ Sphere' 1 6 6
        
        return sim { simTheta = theta + 0.1 }
    
    initSimulation sim = do
        prog <- newProgram [$here|
            // -- vertex shader
            varying vec3 pos;
            void main() {
                vec4 mv = gl_ModelViewMatrix * gl_Vertex;
                pos = vec3(mv);
                gl_Position = gl_ProjectionMatrix * mv;
            }
        |] [$here|
            // -- fragment shader
            varying vec3 pos;
            void main() {
                float dd = pos.x * pos.x + pos.y * pos.y + pos.z * pos.z;
                if (dd > 4 * 4) {
                    gl_FragColor = vec4(1,0,0,1);
                }
                else {
                    gl_FragColor = vec4(0,1,0,1);
                }
            }
        |]
        return $ sim { simShader = prog }

drawFloor :: IO ()
drawFloor = renderPrimitive Lines $ do
    forM_ [ -5 .. 5 ] $ \i ->
        mapM_ vertex [
            vertex3f i (-5) 0, vertex3f i 5 0, -- x-lines
            vertex3f (-5) i 0, vertex3f 5 i 0 -- y-lines
        ]

main :: IO ()
main = runSimulation sim where
    sim = SphereSim { simTheta = 0.0, simShader = undefined }
