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
                
                vec4 p = gl_ModelViewMatrix * gl_Vertex;
                point = vec3(p);
                gl_Position = gl_ProjectionMatrix * p;
            }
        |] [$here|
            // -- fragment shader
            varying vec3 cameraPos;
            varying vec3 point;
            const float r = 0.5; // radius
            
            float solve(vec3 cam, vec3 ray) {
                // solve for the intersection of a ray with the sphere
                // mathematics shamelessly lifted from lecture notes
                float a = dot(ray,ray);
                float b = 2.0 * dot(ray,cam);
                float c = dot(cam,cam) - r * r;
                float det = b * b - 4.0 * a * c;
                
                if (det < 0.0) return -1;
                float t = (-b - sqrt(det)) / (2.0 * a);
                return t;
            }
            
            void main() {
                vec3 ray = normalize(cameraPos - point);
                float t = solve(cameraPos,ray);
                vec3 p = cameraPos + t * ray;
                if (t < 0) discard;
                if (dot(p,p) > r * r + 0.1) discard;
                
                vec3 pnorm = normalize(p * vec3(2,-2,2));
                if (dot(pnorm,ray) > 0.0) pnorm *= -1;
                
                vec3 lightSource = vec3(0,0,5);
                
                float v = dot(pnorm,lightSource);
                gl_FragColor = vec4(v,v,v,1);
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
