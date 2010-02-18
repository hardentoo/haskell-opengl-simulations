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
        mat <- cameraMatrix <$> getCamera
        let [x,y,z] = take 3 $ toList $ mat <> (4 |> [0,0,0,1])
        liftIO $ do
            withProgram prog $ preservingMatrix $ do
                bindProgram prog "camera" $ vertex3f x y z
                --translateM $ vector3f 0 0 2
                color3fM 0 1 1
                renderObject Solid $ Sphere' 4 6 6
    
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
    uniform vec3 camera;
    varying vec3 ray;
    
    void main() {
        vec3 mv = vec3(gl_ModelViewMatrix * gl_Vertex);
        ray = normalize(camera - mv);
        gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; 
    }
|]

fragmentShader :: String
fragmentShader = [$here|
    // -- fragment shader
    uniform vec3 camera;
    varying vec3 ray;
    
    void main() {
        // P(t) = C + t * D, t >= 0
        vec3 C = camera;
        vec3 D = ray;
        float r = 0.5; // radius
        
        float a = dot(D,D);
        float b = 2.0 * dot(D,C);
        float c = dot(C,C) - r * r;
        float det = b * b - 4.0 * a * c;
        
        if (det <= 0.0) {
            gl_FragColor = vec4(0.0,0.0,0.0,1.0);
        }
        else {
            float t = (-b - sqrt(det)) / (2.0 * a);
            vec3 p = C + t * D;
            
            if (dot(p,p) > r * r + 0.01) discard;
            
            vec3 norm = normalize(p * vec3(2.0,-2.0,2.0));
            vec3 lightSource = normalize(vec3(3.0,0.0,-5.0));
            
            // specular + diffuse lightning
            float diffuse = min(max(dot(norm,lightSource),0.0),1.0);
            
            vec3 toCam = normalize(vec3(C)-p);
            vec3 h = normalize(toCam + lightSource);
            float spec = min(max(dot(norm,h),0.0),1.0);
            
            float v = min(max(pow(spec, 100.0) + diffuse, 0.0), 1.0);
            gl_FragColor = vec4(v,v,v,1.0);
        }
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
