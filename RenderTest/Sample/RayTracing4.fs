module RayTracing4

open Engine.Core.Film
open Engine.Core.Scene


let DoRayTrace4() =
    let scenedefine =
        """
<Scene version="0.1">
    <Camera type="pinhole">
          <Point name="position" value="0,1,3"/>
          <Vector name="direction" value="0,0,-1"/>
          <float name="fov" value="120"/>
          <float name="aspectratio" value="1.0"/>
    </Camera>
    <Models>
        <Model type="obj" name="cornellbox">
          <string name="filename" value="CornellBox-Original.obj"/>
        </Model>
    </Models>
    <Materials>
        <Material type="lambert">
            <color name="albedo" value="0.725,0.71,0.68"></color>
        </Material>
        <Material type="lambert">
            <color name="albedo" value="0.14,0.45,0.091"></color>
        </Material>
        <Material type="lambert">
            <color name="albedo" value="0.63,0.065,0.05"></color>
        </Material>
    </Materials>
    <Shapes>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.floor"/>
            <int name="material" value="0"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.ceiling"/>
            <int name="material" value="0"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.backWall"/>
            <int name="material" value="0"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.rightWall"/>
            <int name="material" value="1"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.leftWall"/>
            <int name="material" value="2"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.shortBox"/>
            <int name="material" value="0"/>
        </Shape>
        <Shape type="shapelist">
            <string name="obj_ref" value="cornellbox.tallBox"/>
            <int name="material" value="0"/>
        </Shape>
    </Shapes>
    <Light type="area">
        <string name="shape_ref" value="cornellbox.light"/>
        <color name="intensity" value="10.0,10.0,10.0"/>
    </Light>
    <Film>
        <int name="width" value="300"/>
        <int name="height" value="300"/>
    </Film>
</Scene>
        """
    let scenestate : SceneState = InitSceneState(scenedefine)
    let scene = new Scene(scenestate)
    let w,h = scene.ScreenSize
    
    let window = new Window(w,h)
    window.Init(scene.Render)
    window.Run()
    window.Close()

            
