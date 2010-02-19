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
        let [x,y,z] = take 3 $ map (!! 3) $ toLists $ inv mat
        --let [x,y,z] = take 3 $ toList $ mat <> (4 |> [0,0,0,1])
        liftIO $ do
            withProgram prog $ preservingMatrix $ do
                bindProgram prog "camera" $ vertex3f x y z
                color3fM 0 1 1
                renderObject Solid $ Sphere' 20 6 6
    
    onKeyDown (Char ' ') = do
        mat <- cameraMatrix <$> getCamera
        let [x,y,z] = take 3 $ toList $ mat <> (4 |> [0,0,0,1])
        liftIO $ print (x,y,z)
    onKeyDown _ = return ()
    
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
    //varying vec3 camera;
    varying vec3 ray;
    
    void main() {
        vec3 geom = vec3(gl_Vertex);
        ray = normalize(geom - camera);
        gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex; 
    }
|]

fragmentShader :: String
fragmentShader = [$here|
    // -- fragment shader
    uniform vec3 camera;
    //varying vec3 camera;
    varying vec3 ray;
    
    struct intersection {
        vec3 point;
        float t;
        vec3 normal;
        float reflectivity;
        vec4 color;
        int surface_index;
        vec3 pad;
    };
    
    intersection sphere_intersect(vec3 E_, vec3 D, vec3 P, float r) {
        vec3 E = E_ - P;
        float a = dot(D,D);
        float b = 2.0 * dot(D,E);
        float c = dot(E,E) - r * r;
        float det = b * b - 4.0 * a * c;
        intersection ix;
        
        if (det < 0.0) {
            ix.t = -1.0;
        }
        else {
            float t1 = (-b - sqrt(det)) / (2.0 * a);
            float t2 = (-b + sqrt(det)) / (2.0 * a);
            if (t1 > 0 && t2 > 0) ix.t = min(t1,t2);
            else ix.t = max(t1,t2);
        }
        ix.point = E + ix.t * D;
        ix.normal = normalize((ix.point + P) * vec3(2.0,2.0,2.0));
        return ix;
    }
    
    intersection cast(vec3 E, vec3 D, int exclude = -1) {
        intersection ix_f;
        ix_f.t = -1.0;
        
        for (int i = 0; i < 2; i++) {
            if (i == exclude) continue;
            intersection ix;
            // reasonable defaults
            ix.color = vec4(1.0, 1.0, 1.0, 1.0);
            ix.reflectivity = 0.0;
            ix.surface_index = i;
            
            if (i == 0) {
                ix = sphere_intersect(E, D, vec3(-1.0, 0.0, 0.0), 1.0);
                ix.color = vec4(1.0, 0.4, 0.4, 1.0);
                ix.reflectivity = 0.0;
            }
            if (i == 1) {
                ix = sphere_intersect(E, D, vec3(1.0, 0.0, 0.0), 0.4);
                ix.color = vec4(0.4, 1.0, 1.0, 1.0);
                ix.reflectivity = 1.0;
            }
            
            // use the closer intersection
            if (ix_f.t < 0.0 || (ix.t >= 0.0 && ix.t < ix_f.t)) ix_f = ix;
        }
        return ix_f;
    }
    
    void main() {
        // P(t) = E + t * D, t >= 0
        
        intersection ix = cast(camera, ray);
        // recursion depth of one for now
        if (ix.reflectivity > 0.0) {
            intersection ix_ = cast(ix.point, ix.normal, ix.surface_index);
            if (ix_.t >= 0.0) { // hit something in the reflection
                //ix.color = vec4(1.0,0.0,0.0,1.0);
                ix.color = ix_.color * ix.reflectivity
                    + ix.color * (1 - ix.reflectivity);
            }
            else { // reflect the background
                ix.color = vec4(0.0,0.0,1.0,1.0);
            }
        }
        
        if (ix.t < 0.0) discard;
        
        vec3 lightSource = normalize(vec3(3.0,0.0,1.0));
        // specular + diffuse lightning
        float diffuse = min(max(dot(ix.normal,lightSource),0.0),1.0);
        
        vec3 toEam = normalize(vec3(camera)-ix.point);
        vec3 h = normalize(toEam + lightSource);
        float spec = min(max(dot(ix.normal,h),0.0),1.0);
        
        float v = min(max(pow(spec, 100.0) + diffuse, 0.0), 1.0);
        gl_FragColor = ix.color * vec4(v,v,v,1.0);
        
        vec4 proj = gl_ProjectionMatrix * vec4(ix.point,1.0);
        float depth = proj.z / proj.w;
        gl_FragDepth = clamp(0.5 + 0.5 * depth, 0.0, 1.0);
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
