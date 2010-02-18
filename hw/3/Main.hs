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
        [x,y,z,w] <- map (!! 3) . toLists . inv . cameraMatrix <$> getCamera
        --liftIO $ bindProgram prog "camera" $ vertex3f x y z
        --liftIO $ withProgram prog $ preservingMatrix $ do
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
    // -- fragment shader
    varying vec3 camera;
    varying vec3 offset;
    
    void main() {
        // P(t) = C + t * D, t >= 0
        vec3 C = camera;
        vec3 D = offset;
        
        float a = 1.0, b = 1.0, c = 1.0, k = -1.0;
        
        // a_, b_, and c_ used to compute quadratic equation
        float a_ = (a * D.x * D.x) + (b * D.y * D.y) + (c * D.z * D.z);
        float b_ = 2.0 * (
            (a * C.x * D.x) + (b * C.y * D.y) + (c * C.z * D.z)
        );
        float c_ = (a * C.x * C.x) + (b * C.y * C.y) + (c * C.z * C.z) - k;
        
        float t = -1.0; // default to non-real answer
        if (b_ * b_ < 4.0 * a_ * c_) discard;
        float t1 = (-b_ + sqrt(b_ * b_ - 4.0 * a_ * c_)) / (2.0 * a_);
        float t2 = (-b_ - sqrt(b_ * b_ - 4.0 * a_ * c_)) / (2.0 * a_);
        
        if (t1 >= 0.0 && t2 >= 0.0) {
            t = min(t1,t2); // two solutions, pick nearest
        }
        else {
            t = max(t1,t2); // one solution, pick >= 0
        }
        vec3 point = C + t * D; // point of intersection
        
        //vec3 norm = normalize(vec3(2 * point.x, 2 * point.y, 2 * point.z));
        //vec4 proj = gl_ProjectionMatrix * vec4(vec3(point),1.0);
        //gl_FragDepth = 0.1; // 0.5 + 0.5 * (proj.z / proj.w);
        
        //gl_FragColor = vec4(norm, 1.0);
        gl_FragColor = vec4(point, 1.0);
    }
|]
 
drawFloor :: HookIO EllipsoidSim ()
drawFloor = do
    texPtr <- checkerTex <$> getSimulation
    
    liftIO $ do
        tex : [] <- genObjectNames 1
        textureBinding Texture2D $= Just tex
        textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
        textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        
        texImage2D
            Nothing -- no cube maps
            NoProxy -- standard texture 2d
            0 -- level 0
            RGBA' -- internal format
            (TextureSize2D 19 19) -- texture size
            0 -- border
            texPtr -- pointer to the blurred texture
        
        color4fM 1 1 1 1
        texture Texture2D $= Enabled
        renderPrimitive Quads $ do
            texCoord2fM 0 0 >> vertex3fM (-5) (-5) 0
            texCoord2fM 0 1 >> vertex3fM 5 (-5) 0
            texCoord2fM 1 1 >> vertex3fM 5 5 0
            texCoord2fM 1 0 >> vertex3fM (-5) 5 0
        texture Texture2D $= Disabled
        deleteObjectNames [tex]

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
