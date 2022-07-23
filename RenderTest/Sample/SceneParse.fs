module SceneParse


open System.IO
open System.Xml
open System.Xml.Linq
open FSharp.Collections
open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.Material
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Core.Ray
open Engine.Model.Obj
open Engine.Model.ObjLoader
open Engine.Core.Shapes.Sphere
open Engine.Core.Shapes.Triangle
open Engine.Core.Shapes.Rect
open Engine.Core.Shapes.Box
open Engine.Core.Shapes.CircleAreaLightObject
open Engine.Core.Samplers.JitteredSampler
open Engine.Core.Materials.Lambertian
open Engine.Core.Materials.GlossySpecular
open Engine.Core.Materials.PerfectSpecular
open Engine.Core.Integrator

open Engine.Core.Accels.BvhNode

open Engine.Core.Film

open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Interfaces.ICamera
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.ISampler
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld
open Engine.Core.Interfaces.IIntegrator
open Engine.Core.Tracers.RayCast
open Engine.Core.Tracers.Whitted
open Engine.Core.Tracers.PathTracer
open Engine.Core.Lights.Ambient
open Engine.Core.Lights.PointLight
open Engine.Core.Lights.Directional
open Engine.Core.Lights.AreaLight

open Engine.Core.Scene

let scenexml =
    """
<Scene Version="0.1">
    <Camera type="pinhole">
      <Point name="position" value="1,1,1"/>
      <Vector name="direction" value="0,0,1"/>
      <float name="fov" value="45"/>
      <float name="aspectratio" value="1.0"/>
    </Camera>
    <Models>
      <Model type="obj" name="box">
          <string name="filename" value="box.obj"/>
      </Model>
    </Models>
    <textures>
      <texture type="const" id="t01">
          <color name="rgb" value="0.8,0.8,0.8"></color>
      </texture>
      <texture type="checkerboard" id="ck01">
        <color name="color0" value=""></color>
        <ref id="t01" name="color1"></ref>
      </texture>
    </textures>
    <Materials>
      <Material type="lambert">
        <ref id="t01" name="albedo"></ref>
      </Material>
    </Materials>
    <Shapes>
        <Shape type="rect|triangle|mesh|sphere|shapelist" id="light_shape">
            <string name="obj_ref" value="box.left"/>
            <int name="material" value="0"/>
        </Shape>
    </Shapes>
    <Lights>
        <Light type="area">
            <string name="shape_ref" value="light_shape"/>
            <color name="intensity" value="1.0,1.0,1.0"/>
        </Light>
    </Lights>
    <Film>
        <int name="width" value="200"/>
        <int name="height" value="200"/>
        <integrators>
        </integrators>
    </Film>
</Scene>
    """

let ParseCamera() =
    let node =
        XElement.Parse("""
        <Camera type="pinhole">
          <Point name="position" value="1,1,1"/>
          <Vector name="direction" value="0,0,1"/>
          <float name="fov" value="45"/>
          <float name="aspectratio" value="1.0"/>
        </Camera>
        """)
    Parse.Camera.ToCamera(node)
let ParseMaterial() =
    let node =
        XElement.Parse("""
        <Material type="lambert">
            <color name="albedo" value="0.8,0.8,0.8"></color>
        </Material>
        """)
    Parse.Material.ToMaterial(node) :> IMaterial

let ParseModel() =
    let node =
        XElement.Parse("""
        <Model type="obj" name="cube">
          <string name="filename" value="cube.obj"/>
        </Model>
        """)
    Parse.Model.ToModel(node)

let ParseShape() =
    let obj = Map<string, ObjState>([ParseModel()])
    let node =
        XElement.Parse("""
        <Shape type="shapelist" id="light_shape">
            <string name="obj_ref" value="cube.default"/>
            <int name="material" value="13"/>
        </Shape>
        """)
    Parse.Shape.ToShape(node, obj)

let ParseFilm() =
    let node =
        XElement.Parse("""
        <Film>
            <int name="width" value="200"/>
            <int name="height" value="200"/>
        </Film>
        """)
    Parse.Film.ToFilm(node)

let ParseLight() =
    let obj = Map<string, ObjState>([ParseModel()])
    let node =
        XElement.Parse("""
        <Light type="area">
            <string name="shape_ref" value="cube.light"/>
            <color name="intensity" value="1.0,1.0,1.0"/>
        </Light>
        """)
    Parse.Lights.ToLight(node, obj)

let TestParse() =
    System.Console.WriteLine(ParseCamera())
    ParseMaterial() |> ignore
    ParseModel() |> ignore
    ParseShape() |> ignore
    ParseFilm()|> ignore
    ParseLight() |> ignore