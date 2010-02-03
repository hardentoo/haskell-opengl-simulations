-- navigation and spheres
{-# LANGUAGE QuasiQuotes #-} -- for heredocs
module Main where
import Graphics.UI.GL.Simulation
import Control.GL.Shader (newProgram,withProgram,bindProgram,here)
import Control.Monad (forM_)
import Data.IORef (IORef,newIORef)

data SphereSim = SphereSim {
    simShader :: Program
}

instance Simulation SphereSim where
    displayWithCamera sim camera = runAtFPS 60 $ do
        let prog = simShader sim
        color3fM 0.8 0.8 1 >> drawFloor
        
        withProgram prog $ preservingMatrix $ do
            color3fM 0 1 1
            renderObject Solid $ Sphere' 1 6 6
        return sim
    
    initSimulation sim = do
        prog <- newProgram [$here|
            // -- vertex shader
            varying vec3 point;
            varying vec3 cameraPos;
            void main() {
                // translation is the third column of the inverse matrix
                cameraPos = vec3(gl_ProjectionMatrixInverse[3]);
                
                point = vec3(gl_ModelViewMatrix * gl_Vertex);
                gl_Position = gl_ProjectionMatrix
                    * gl_ModelViewMatrix * gl_Vertex;
            }
        |] [$here|
            // -- fragment shader
            varying vec3 cameraPos;
            varying vec3 point;
            void main() {
                vec3 ray = normalize(cameraPos - point);
                // solve for the intersection of the ray with the sphere
                // mathematics shamelessly lifted from lecture notes
                float r = 0.5;
                float a = dot(ray,ray);
                float b = 2.0 * dot(ray,cameraPos);
                float c = dot(cameraPos,cameraPos) - r * r;
                float det = b * b - 4.0 * a * c;
                // edges are noisy still somehow...
                if (det < 0.0) discard;
                float t = (-b - sqrt(det)) / (2.0 * a);
                vec3 p = cameraPos + t * ray;
                if (dot(p,p) > r * r + 0.1) discard;
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
    sim = SphereSim { simShader = undefined }
