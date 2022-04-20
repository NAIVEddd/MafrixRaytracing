module Assignment1_6_837

open OpenCvSharp
open Engine.Core.Color
open Engine.Core.Ray
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Model.Obj
open FParsec


let keyword_OrthographicCamera : Parser<_, unit> = pstring "OrthographicCamera"
let keyword_Background : Parser<_, unit> = pstring "Background"
let keyword_Materials : Parser<_, unit> = pstring "Materials"
let keyword_Material : Parser<_, unit> = pstring "PhongMaterial"
let keyword_Group : Parser<_, unit> = pstring "Group"
let keyword_Sphere : Parser<_, unit> = pstring "Sphere"
let keyword_Center : Parser<_, unit> = pstring "center"
let keyword_Direction : Parser<_, unit> = pstring "direction"
let keyword_Up : Parser<_, unit> = pstring "up"
let keyword_Size : Parser<_, unit> = pstring "size"
let keyword_Color : Parser<_, unit> = pstring "color"
let keyword_DiffuseColor : Parser<_, unit> = pstring "diffuseColor"
let keyword_NumMaterials : Parser<_, unit> = pstring "numMaterials"
let keyword_NumObjects : Parser<_, unit> = pstring "numObjects"
let keyword_Radius : Parser<_, unit> = pstring "radius"
let keyword_MaterialIndex : Parser<_, unit> = pstring "MaterialIndex"
let pfloat3 = many (pfloat .>> spaces)
let toColor (f3:float list) =
    assert(f3.Length = 3)
    Color(f3[0],f3[1],f3[2])
let toPoint (f3:float list) =
    assert(f3.Length = 3)
    Point(f3[0],f3[1],f3[2])
let toVector (f3:float list) =
    assert(f3.Length = 3)
    Vector(f3[0],f3[1],f3[2])
let pColor = pfloat3 |>> toColor
let pPoint = pfloat3 |>> toPoint
let pVector = pfloat3 |>> toVector
let beginScope = spaces >>. pstring "{" .>> spaces
let endScope = spaces >>. pstring "}" .>> spaces

let lerp f p1 p2 = p1 + f * (p2 - p1)
[<Struct>]
type OrthographicCamera =
    val center : Point
    val direction : Vector
    val horizontal : Vector
    val up : Vector
    val size : int
    val leftBottom : Point
    new(c, d:Vector, u:Vector, size) =
        let dir = d.Normalize
        let hori = dir.Cross(u.Normalize)
        let up = hori.Cross(dir)
        {
            center = c;
            direction = dir;
            up = float size * up;
            horizontal = float size * hori;
            size = size;
            leftBottom = c - (float size * up)/2.0 - (float size * hori)/2.0;
        }
    member this.GetRay(p:Point2D) =
        let pos = this.leftBottom + p.x * this.horizontal + p.y * this.up
        Ray(pos, this.direction)
        
[<Struct>]
type AssetMaterial =
    val color : Color
    new(c) =
        {
            color = c
        }
[<Struct>]
type Hit =
    val hit : bool
    val material : int
    val t : float
    val point : Point
    val ray: Ray
    new(t,r,mat) = {hit=true;t = t;material=mat;ray=r;point=r.PointAtParameter(t);}
type Sphere =
    struct
        val center: Point
        val radius: float
        val materialIdx: int
        new(c:Point, r:float, mate) = {center = c; radius = r; materialIdx = mate}
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            let oc = r.Origin() - this.center
            let a = r.Direction().Dot(r.Direction())
            let b = 2.0 * oc.Dot(r.Direction())
            let c = oc.Dot(oc) - this.radius*this.radius
            let discriminant = b*b-4.0*a*c
            if discriminant > 0 then
                let tmp = (-b - sqrt(discriminant))/(2.0*a)
                if tmp < tMax && tmp > tMin then
                    let p = r.PointAtParameter(tmp)
                    Hit(tmp,r,this.materialIdx)
                    //HitRecord(true, tmp, p, (p-this.center)/this.radius, r, Some this.material)
                else
                    let tmp = (-b + sqrt(discriminant))/(2.0*a)
                    if tmp < tMax && tmp > tMin then
                        Hit(tmp,r,this.materialIdx)

                        //let p = r.PointAtParameter(tmp)
                        //HitRecord(true, tmp, p, (p-this.center)/this.radius, r, Some this.material)
                    else
                        Hit()
            else
                Hit()
    end
type Scene =
    struct
        val camera : OrthographicCamera
        val backgroundColor : Color
        val materials : AssetMaterial list
        val objs : Sphere list
        new(cam,c,mats,objs) = {camera=cam;backgroundColor=c;materials=mats;objs=objs;}
        member this.GetColorDepth(p:Point2D) =
            let tmin = 0.000001
            let tmax = 9999999.
            let ray = this.camera.GetRay(p)
            let hit = this.objs |> List.map (fun item -> item.Intersect(ray, tmin, tmax)) |>
                        List.minBy(fun hit -> if hit.hit then hit.t else tmax)
            if hit.hit then
                let matIdx = hit.material
                this.materials[matIdx].color, hit.t
            else
                this.backgroundColor, 0.
    end
let toOrthographicCamera (((c,dir),up),size) =
    OrthographicCamera(c,dir,up,size)
let pOrthographicCameraContent =
    keyword_Center .>> spaces >>. pPoint .>> spaces .>>
    keyword_Direction .>> spaces .>>. pVector .>>
    keyword_Up .>> spaces .>>. pVector .>>
    keyword_Size .>> spaces .>>. pint32 .>> spaces |>> toOrthographicCamera

let pMaterial =
    keyword_Material >>.
    between beginScope endScope
            (keyword_DiffuseColor .>>spaces >>. pColor |>> AssetMaterial)

let toIdxSphere (i, (c,r)) = Sphere(c,r,i)
let toIdxSpheres (i, l) = l |> List.map (fun (c,r) -> Sphere(c,r,i))
let toSphere (c, r) = Sphere(c,r,0)
let toScene (((cam,c),mats),objs) =
    Scene(cam,c,mats,objs)

let pNoIdxSphere =
    keyword_Sphere >>.
    between beginScope endScope
            (keyword_Center >>. spaces >>. pPoint .>> spaces .>> keyword_Radius .>> spaces .>>. pfloat)
let pIdxSphere =
    keyword_MaterialIndex >>. spaces >>. pint32 .>> spaces .>>.
    many pNoIdxSphere |>> toIdxSpheres
let pSpheres = many pIdxSphere |>> List.concat
let pNumObjects =
    keyword_NumObjects .>> spaces >>. pint32 .>> spaces


let pOrthographicCamera =
    keyword_OrthographicCamera >>.
    between beginScope endScope
            pOrthographicCameraContent
let pBackground =
    keyword_Background >>.
    between beginScope endScope
            (keyword_Color .>> spaces >>. pColor)
let pMaterials =
    keyword_Materials >>.
    between beginScope endScope
            (keyword_NumMaterials .>> spaces >>. pint32 .>> spaces >>.
                many pMaterial)
let pGroup =
    keyword_Group >>.
    between beginScope endScope
            (pNumObjects >>. pSpheres)
let pScene =
    spaces >>. pOrthographicCamera .>>
    spaces .>>. pBackground .>>
    spaces .>>. pMaterials .>>
    spaces .>>. pGroup |>> toScene


let test p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg
        assert(false)
        Scene(OrthographicCamera(Point(),Vector(),Vector(),0),Color(), [], [])

[<Struct>]
type RayTracer_Info =
    val inputFile: string
    val screenSize : int
    val minDepth : float
    val maxDepth : float
    new(command:string) =
        let ls = command.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        {
            inputFile = ls[2];
            screenSize = int ls[4];
            minDepth = float ls[9];
            maxDepth = float ls[10];
        }
let LoadScene() =
    // Source file(eg. scene1_01.txt) from https://groups.csail.mit.edu/graphics/classes/6.837/F04/assignments/assignment1/
    let info = RayTracer_Info("raytracer -input scene1_02.txt -size 200 200 -output output1_02.tga -depth 8 12 depth1_02.tga")
    let screen_size = info.screenSize
    let mutable lines = System.IO.File.ReadAllText(info.inputFile)
    //test keyword_OrthographicCamera lines
    let scene = test pScene lines
    let screen = Screen(screen_size, screen_size)

    // render begin
    for x in 0..screen_size-1 do
        for y in 0..screen_size-1 do
            let p = Point2D(float x / float screen_size, float (screen_size-y) / float screen_size)
            screen[x,y] <- scene.GetColorDepth(p)
    //render end

    let depthField = info.maxDepth - info.minDepth
    let mat = new Mat(Size(screen_size, screen_size), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    let (w,h) = screen.Size()
    for y in 0..h-1 do
        for x in 0..w-1 do
            let c = screen.Pixel(x,y) * 255.
            //let c = (1. - (screen.Depth(x,y) - info.minDepth) / depthField) * Color(255,255,255)    // Depth
            let vec = Vec3b(byte c.b, byte c.g, byte c.r)
            indexer[y,x] <- vec
    Cv2.ImShow("Assignment 1", mat)
    Cv2.WaitKey() |> ignore