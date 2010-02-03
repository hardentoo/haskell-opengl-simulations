-- Raytrace a sphere with shaders and a crazy new simulator typeclass
-- James Halliday

{-# LANGUAGE QuasiQuotes #-} -- for magical heredocs
module Main where
import Graphics.UI.GL.Simulation
import Control.GL.Shader (newProgram,withProgram,bindProgram,here)
import Control.Monad (forM_)
import Data.IORef (IORef,newIORef)

-- With the simulator typeclass, just create your own datatypes...
data SphereSim = SphereSim {
    simShader :: Program
}

-- ...and then create instances with your type defining callbacks and such 
instance Simulation SphereSim where
    display sim = runAtFPS 60 $ do
        let prog = simShader sim
        color3fM 0.8 0.8 1 >> drawFloor
        
        -- It turns out that the camera position can be taken right out of the
        -- projection matrix, so no need to bind any uniform variables.
        withProgram prog $ preservingMatrix $ do
            color3fM 0 1 1
            renderObject Solid $ Sphere' 1 6 6
        return sim
    
    initSimulation sim = do
        -- Shaders are fairly abstracted away now too.
        -- This callback runs right after the window is initialized, since
        -- trying to build a shader before then segfaults mysteriously.
        prog <- newProgram [$here|
            // -- vertex shader
            varying vec3 point;
            varying vec3 cameraPos;
            void main() {
                // Translation is the third column of the inverse matrix.
                // An amazing trick that I by no means came up with!
                cameraPos = vec3(gl_ProjectionMatrixInverse[3]);
                
                vec4 p = gl_ModelViewMatrix * gl_Vertex;
                point = vec3(p);
                gl_Position = gl_ProjectionMatrix * p;
            }
        |] [$here|
            // -- fragment shader
            varying vec3 cameraPos;
            varying vec3 point;
            
            void main() {
                // Solve for the intersection of a ray with the sphere.
                // Mathematics shamelessly lifted from lecture notes.
                // It mostly works even.
                vec3 ray = normalize(cameraPos - point);
                float r = 0.5; // radius
                
                float a = dot(ray,ray);
                float b = 2.0 * dot(ray,cameraPos);
                float c = dot(cameraPos,cameraPos) - r * r;
                float det = b * b - 4.0 * a * c;
                
                if (det <= 0.0) discard;
                float t = (-b - sqrt(det)) / (2.0 * a);
                vec3 p = cameraPos + t * ray;
                
                // problem with shrinking spheres without the extra +0.5
                if (dot(p,p) > r * r + 0.5) discard;
                
                vec3 norm = normalize(p * vec3(2,-2,2));
                vec3 lightSource = normalize(vec3(3,0,-5));
                
                // specular + diffuse lightning
                float diffuse = min(max(dot(norm,lightSource),0.0),1.0);
                
                vec3 toCam = normalize(vec3(cameraPos)-p);
                vec3 h = normalize(toCam + lightSource);
                float spec = min(max(dot(norm,h),0.0),1.0);
                
                float v = min(max(pow(spec, 3) + diffuse, 0.0), 1.0);
                gl_FragColor = vec4(v,v,v,1);
            }
        |]
        return $ sim { simShader = prog }

-- Draw a floor, obviously!
drawFloor :: IO ()
drawFloor = renderPrimitive Lines $ do
    forM_ [ -5.0, -4.5 .. 5.0 ] $ \i ->
        mapM_ vertex [
            vertex3f i (-5) 0, vertex3f i 5 0, -- x-lines
            vertex3f (-5) i 0, vertex3f 5 i 0 -- y-lines
        ]

main :: IO () -- what a tiny main!
main = do
    putStrLn "*** Haskell OpenGL Simulator Magic ***\n\
        \    Keys:\n\
        \        forward => w, back => a,\n\
        \        left => s, right => d\n\
        \        up => q, down => z\n\
        \    Rotate: drag left mouse button\n\
        \    Skewer: drag right mouse button\n\
        \ "
    runSimulation $ SphereSim { simShader = undefined }
