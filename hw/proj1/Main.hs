{-# LANGUAGE QuasiQuotes #-} -- for heredocs
module Main where
import Graphics.UI.Simulation3D

import Foreign (newArray)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))

-- With the simulator typeclass, just create your own datatypes...
data EllipsoidSim = EmptySim | EllipsoidSim {
    simShader :: Program,
    checkerTex :: PixelData (Color4 GLfloat)
}

-- ...and then create instances with your type defining callbacks and such 
instance Simulation EllipsoidSim where
    navigator = wasd $ WASD { rSpeed = 0.001, tSpeed = 0.05 }
    
    display = do
        prog <- simShader <$> getSimulation
        --[x,y,z,_] <- map (!! 3) . toLists . inv . cameraMatrix <$> getCamera
        --liftIO $ bindProgram prog "camera" $ vertex3f x y z
        liftIO $ withProgram prog $ preservingMatrix $ do
            renderObject Solid $ Sphere' 1.5 6 6
    
    begin = do
        ptr <- liftIO $ newArray $ take (19 * 19)
            $ cycle [ color4f 0 0 0 0.5, color4f 1 1 1 0.5 ]
        prog <- liftIO $ newProgram vertexShader fragmentShader
        
        setWindowBG $ color4cf 0.8 0.8 1 1
        setSimulation $ EllipsoidSim {
            simShader = prog,
            checkerTex = PixelData RGBA Float ptr
        }

vertexShader :: String
vertexShader = [$here|
    // -- vertex shader
    varying vec3 offset; // -- normalized vector offset of camera
    varying vec3 camera; // -- camera in world coords
    
    void main() {
        vec3 mv = vec3(gl_ModelViewMatrix * gl_Vertex);
        camera = vec3(gl_ModelViewMatrixInverse[3]);
        offset = normalize(camera - mv);
        gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
    }
|]

fragmentShader :: String
fragmentShader = [$here|
    varying vec3 camera;
    varying vec3 offset;
    
    #define surface(C,D,T) \
        C.x + T
    
    void main() {
        // P(t) = C + t * D, t >= 0
        vec3 C = camera;
        vec3 D = offset;
        
        float tn_prev = 0.0;
        float tn = 1.0;
        float v;
        
        for (int i = 0; i < 4; i++) {
            float s = surface(C,D,tn);
            float sp = surface(C,D,tn_prev);
            v = (tn - tn_prev) / (s - sp) * sp;
            tn_prev = tn;
            tn = tn - v;
        }
        
        if (abs(v) > 0.01) discard;
         
        vec3 point = C + tn * D; // point of intersection
        vec4 proj = gl_ProjectionMatrix * vec4(vec3(point),1.0);
        gl_FragDepth = 0.1; // 0.5 + 0.5 * (proj.z / proj.w);
        gl_FragColor = vec4(1.0,0.0,0.0,1.0);
    }
|]
 
main :: IO ()
main = do
    putStrLn "*** Haskell OpenGL Simulator Magic ***\n\
        \    To quit, press escape.\n\
        \    Keys:\n\
        \        forward => w, back => a,\n\
        \        left => s, right => d\n\
        \        up => q, down => z\n\
        \    Rotate: drag left mouse button\n\
        \    Skewer: drag right mouse button\n\
        \ "
    runSimulation EmptySim
