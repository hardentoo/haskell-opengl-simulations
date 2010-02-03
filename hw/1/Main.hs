-- navigation and spheres
{-# LANGUAGE QuasiQuotes #-} -- for heredocs
module Main where
import Graphics.UI.GL.Simulation
import Control.GL.Shader (newProgram,withProgram,bindProgram,here)
import Control.Monad (forM_)
import Data.IORef (IORef,newIORef)

data SphereSim = SphereSim {
    simTheta :: GLfloat,
    simShader :: Program
}

instance Simulation SphereSim where
    displayWithCamera sim camera = runAtFPS 60 $ do
        let theta = simTheta sim; prog = simShader sim
        color3fM 0.8 0.8 1 >> drawFloor
        
        let Vertex3 cx cy cz = cameraPos camera
        bindProgram prog "cameraPos" $ vertex3f cx cy cz
        bindProgram prog "spherePos" $ vertex3f 0 0 0
        
        withProgram prog $ preservingMatrix $ do
            color3fM 0 1 1
            --translate $ vector3f 0 0 0
            --rotate theta $ vector3f 0 0 1
            renderObject Solid $ Sphere' 1 6 6
        return sim { simTheta = theta + 0.5 }
    
    initSimulation sim = do
        prog <- newProgram [$here|
            // -- vertex shader
            varying vec3 point;
            void main() {
                vec4 p = gl_ModelViewMatrix * gl_Vertex;
                point = vec3(p);
                gl_Position = gl_ProjectionMatrix * p;
            }
        |] [$here|
            // -- fragment shader
            uniform vec3 spherePos;
            uniform vec3 cameraPos;
            varying vec3 point;
            void main() {
                vec3 ray = normalize(cameraPos - point);
                // solve for the intersection of the ray with the sphere
                // mathematics shamelessly lifted from lecture notes
                float r = 0.5;
                float a = dot(ray,ray);
                float b = 2.0*dot(ray,cameraPos);
                float c = dot(cameraPos,cameraPos) - r * r;
                float det = b * b - 4.0 * a * c;
                // if (det < 0.0) discard;
                float t = (-b - sqrt(det)) / (2.0 * a);
                vec3 p = cameraPos + t * ray;
                // if (dot(p,p) == 0) discard;
                gl_FragColor = vec4(cameraPos,1);
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
