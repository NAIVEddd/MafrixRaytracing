module Engine.Core.Scene
open Engine.Core.Ray
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Light
open Engine.Core.Accels.BvhNode
open Engine.Core.Material
open Engine.Core.Camera
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.ICamera
open Engine.Core.Interfaces.IIntegrator
open System.Xml.Linq
open Engine.Core.Shapes.Triangle
open Engine.Core.Shapes.Sphere
open Engine.Core.Shapes.Rect
open Engine.Core.Film
open Engine.Core.Texture
open Engine.Core.Integrator
open Engine.Model.ObjLoader


module Parse =
    module Utils =
        let ToFloat(node : XElement) =
            let valueattr = node.Attribute("value")
            assert(not (isNull valueattr.Value))
            float valueattr.Value
        let ToInt(node : XElement) =
            let v = node.Attribute("value")
            assert(not (isNull v.Value))
            int v.Value
        let ToFloat3(node : XElement) =
            let v = node.Attribute("value").Value
            let vs = v.Split(',', System.StringSplitOptions.TrimEntries &&& System.StringSplitOptions.RemoveEmptyEntries)
            let fs = vs |> Array.map(fun f -> float(f))
            assert(vs.Length = 3)
            assert(fs.Length = 3)
            fs
        let ToPoint(node : XElement) =
            let fs = ToFloat3(node)
            Point(fs[0], fs[1], fs[2])
        let ToVector(node: XElement) =
            let fs = ToFloat3(node)
            Vector(fs[0], fs[1], fs[2])
        let ToColor(node: XElement) =
            let fs = ToFloat3(node)
            Color(fs[0], fs[1], fs[2])
        let ToString(node : XElement) =
            let v = node.Attribute("value").Value
            assert(not (isNull v))
            v

    module Camera =
        let ToPinhole(node : XElement) =
            let mutable position = Point()
            let mutable direction = Vector()
            let mutable fov = 60.
            let mutable aspect = 1.333
            for n in node.Elements() do
                let argname = n.Attribute("name")
                assert(not (isNull argname))
                match argname.Value with
                | "position" -> position <- Utils.ToPoint(n)
                | "direction" -> direction <- Utils.ToVector(n)
                | "fov" -> fov <- Utils.ToFloat(n)
                | "aspectratio" -> aspect <- Utils.ToFloat(n)
                | _ -> assert(false)
            new PinholeCamera(position, direction, fov, aspect)
        let ToCamera(node : XElement) =
            let cameratype = node.Attribute("type")
            assert(cameratype.Value = "pinhole")
            ToPinhole(node)

    module Material =
        let ToLambert(node: XElement) =
            let mutable albedo = Color()
            for n in node.Elements() do
                let argname = n.Attribute("name")
                assert(not (isNull argname))
                match argname.Value with
                | "albedo" -> albedo <- Utils.ToColor(n)
                | _ -> assert(false)
            new Lambertian(albedo)
        let ToMaterial(node : XElement) =
            let materialtype = node.Attribute("type")
            assert(materialtype.Value = "lambert")
            ToLambert(node)

        let ToMaterials(node : XElement) =
            assert(node.Name.ToString() = "Materials")
            let materials = ResizeArray<IMaterial>()

            for n in node.Elements() do
                let material = ToMaterial(n)
                materials.Add(material)

            materials.ToArray()

    module Model =
        let ToObjModel(node : XElement) =
            let v = node.Attribute("name")
            assert(not (isNull v))
            let objname = v.Value

            let mutable filename = ""
            for n in node.Elements() do
                let argname = n.Attribute("name")
                assert(not (isNull argname))
                match argname.Value with
                | "filename" -> filename <- Utils.ToString(n)
                | _ -> assert(false)
            let model = LoadObjModel(filename)

            (objname, model)
        let ToModel(node : XElement) =
            assert(node.Name.ToString() = "Model")

            let modeltype = node.Attribute("type")
            assert(modeltype.Value = "obj")
            
            ToObjModel(node)

        let ToModels(node : XElement) =
            assert(node.Name.ToString() = "Models")
            let models = ResizeArray<string * ObjState>()

            for n in node.Elements() do
                let model = ToModel(n)
                models.Add(model)

            new Map<string, ObjState>(models)

    module Shape =
        let FindModel(k : string, m : Map<string, ObjState>) =
            let nm = k.Split('.', System.StringSplitOptions.TrimEntries &&& System.StringSplitOptions.RemoveEmptyEntries)
            assert(nm.Length = 2)
            let objref = (Map.find nm[0] m).facesMaps
            Map.find nm[1] objref
        let ToShapeList(node : XElement, m : Map<string, ObjState>) =
            let mutable objref = ""
            let mutable material = 0

            for n in node.Elements() do
                let argname = n.Attribute("name")
                match argname.Value with
                | "obj_ref" -> objref <- Utils.ToString(n)
                | "material" -> material <- Utils.ToInt(n)
                | _ -> assert(false)

            let objs = FindModel(objref, m).ToArray()
            objs |> Array.map (fun x ->
                match x with
                | :? Triangle as tri -> Triangle(tri.v0, tri.v1, tri.v2, material) :> IHitable
                | :? Rect as rect -> Rect(rect.trig1.v0, rect.trig1.v1, rect.trig1.v2, rect.trig2.v2, material)
                | :? Sphere as sph -> Sphere(sph.center, sph.radius, material)
                | _ -> assert(false); Sphere()
            )

        let ToShape(node : XElement, m : Map<string, ObjState>) =
            let shapetype = node.Attribute("type")
            assert(shapetype.Value = "shapelist")
            ToShapeList(node, m)

        let ToShapes(node : XElement, m: Map<string, ObjState>) =
            assert(node.Name.ToString() = "Shapes")

            let shapes = ResizeArray<IHitable>()
            for n in node.Elements() do
                let shape = ToShape(n, m)
                for s in shape do
                    shapes.Add(s)

            shapes.ToArray()

    module Lights =
        let ToAreaLight(node: XElement, m : Map<string, ObjState>) : INewLight =
            let mutable objref = ""
            let mutable intensity = Color()

            for n in node.Elements() do
                let argname = n.Attribute("name")
                match argname.Value with
                | "shape_ref" -> objref <- Utils.ToString(n)
                | "intensity" -> intensity <- Utils.ToColor(n)
                | _ -> assert(false)

            let obj = Shape.FindModel(objref, m).ToArray()[0]
            match obj with
            | :? Rect as rect -> NewAreaLight(rect.trig1.v0, rect.trig1.v1, rect.trig1.v2, rect.trig2.v2, rect.trig1.normal, intensity)
            | _ -> assert(false); NewAreaLight(Point(-0.24,1.98,0.16),Point(-0.24,1.98,-0.22),Point(0.23,1.98,-0.22),Point(0.23,1.98,0.16),Vector(0,-1,0),Color(20,20,20))

        let ToLight(node : XElement, m : Map<string, ObjState>) : INewLight =
            let lighttype = node.Attribute("type")
            assert(lighttype.Value = "area")
            ToAreaLight(node, m)

    module Film =
        let ToFilm(node : XElement) =
            let mutable width = 800
            let mutable height = 800
            for n in node.Elements() do
                let argname = n.Attribute("name")
                match argname.Value with
                | "width" -> width <- Utils.ToInt(n)
                | "height" -> height <- Utils.ToInt(n)
                | _ -> assert(false)
            new Film(width, height)

    module SceneState =
        type State =
            val materials : IMaterial[]
            val mutable camera : ICamera
            val mutable light : INewLight
            val shapes : IHitable[]
            val film : Film
            new(mats : IMaterial[],
                shapes : IHitable[],
                cam:ICamera, arealight: INewLight, f : Film) =
                {
                    materials = mats;
                    shapes = shapes;
                    camera = cam;
                    light = arealight;
                    film = f;
                }

        let GetSceneState(node:XElement) =
            let defaultnode = new XElement("Invalid_XElement")
            let mutable cameranode = defaultnode
            let mutable modelsnode = defaultnode
            let mutable materialsnode = defaultnode
            let mutable shapesnode = defaultnode
            let mutable lightnode = defaultnode
            let mutable filmnode = defaultnode

            for n in node.Elements() do
                let nodename = n.Name.ToString()
                match nodename with
                | "Camera" -> cameranode <- n
                | "Models" -> modelsnode <- n
                | "Materials" -> materialsnode <- n
                | "Shapes" -> shapesnode <- n
                | "Light" -> lightnode <- n
                | "Film" -> filmnode <- n
                | _ -> assert(false)

            let camera = Camera.ToCamera(cameranode)
            let m = Model.ToModels(modelsnode)
            let light = Lights.ToLight(lightnode, m)
            let film = Film.ToFilm(filmnode)
            let materials = Material.ToMaterials(materialsnode)
            let shapes = Shape.ToShapes(shapesnode, m)

            for material in materials do
                MaterialManager.GetManager().Add(material) |> ignore

            new State(materials, shapes, camera, light, film)

            
type SceneState = Parse.SceneState.State
let InitSceneState(filename : string) : SceneState =
    let node = XElement.Parse(filename)
    //let node = XElement.Load(filename)
    let version = node.Attribute("version")
    // this scene loader only support version 0.1
    assert(version.Value = "0.1")
    Parse.SceneState.GetSceneState(node)

let clamp(x:float) = if x < 0. then 0. elif x > 1. then 1. else x
let saturate(col:Color) =
    let r = clamp(col.r)
    let g = clamp(col.g)
    let b = clamp(col.b)
    Color(r,g,b)

let ACESFilmToneMapping(x:Color) =
    // Source code from:
    //      https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
    let a = 2.51
    let b = 0.03
    let c = 2.43
    let d = 0.59
    let e = 0.14
    let col = ((x*(a*x+b))/(x*(c*x+d)+e))
    saturate(col)

type Scene =
    val camera : ICamera
    val film : Film
    val width : int
    val height : int
    val pixelIntegrator : PixelIntegrator
    val pixelCoords : (int*int) array
    new(state : SceneState) =
        let AreaLight = state.light
        //let result = LoadObjModel("CornellBox-Original.obj")
        let objs = state.shapes
        let bvh = Bvh.Build(objs)
        
        let trace : IPathTracer = new PathIntegrator(bvh,3,AreaLight)
        let cam = state.camera
        let w, h = state.film.Size
        {
            camera = cam;
            film = state.film;
            width = w; height = h;
            pixelIntegrator = new PixelIntegrator(w, h, cam, trace)
            pixelCoords = state.film.PixelCoords;
        }
    member this.ScreenSize with get() = (this.width, this.height)
    member this.PostProcessAndToScreenBuffer(texture:Texture2D<Color>, buffer:byte array) =
        let width = this.width * 4
        this.pixelCoords |> Array.iter(fun(x,y) ->
            let mutable col = texture[x,y]
            col <- ACESFilmToneMapping(col)
            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
            let ir = int (255.99*col.r)
            let ig = int (255.99*col.g)
            let ib = int (255.99*col.b)
            col <- Color(ir,ig,ib)
            texture[x,y] <- col
            buffer[x*4 + y * width] <- byte col.r
            buffer[x*4 + 1 + y * width] <- byte col.g
            buffer[x*4 + 2 + y * width] <- byte col.b
            buffer[x*4 + 3 + y * width] <- byte 255
        )
    member this.Render(delta:float, buffer : byte array) =
        let frame = this.film.GetFrame(this.pixelIntegrator, 1)
        this.PostProcessAndToScreenBuffer(frame, buffer)

// type World(_items:IHitable[], amb:ILight, _lights:ILight[]) =
//     let mutable items = _items
//     let mutable bvh = Unchecked.defaultof<BvhNode>
//     let mutable lights = _lights
//     let mutable tracer = Unchecked.defaultof<ITracer>
//     interface IWorld with
//         member this.BoundBox(t0:float,t1:float) =
//                 let pmin = Vector()
//                 let pmax = Vector(1,1,1)
//                 true, AABB(pmin,pmax)
//         member this.ShadowHit(ray:Ray) =
//             let record = (this:IHitable).Hit(ray, 0.00001, 99999999.0)
//             if record.bHit then
//                 true, record.t
//             else
//                 false, 0.0
//         member this.Hit(r:Ray, tMin:float, tMax:float) = bvh.Hit(r,tMin,tMax)
//         member this.GetAmbientLight() = amb
//         member this.GetLights() = lights
//         member this.AddLight(light) = lights <- Array.append lights [|light|]
//         member this.GetObjects() = items
//         member this.AddObject(obj) = items <- Array.append items [|obj|]
//         member this.Build() = bvh <- BuildBvhNode(items, items.Length, 0, 1) :?> BvhNode
//         member this.SetTracer(t) = tracer <- t
//         member this.GetTracer() = tracer