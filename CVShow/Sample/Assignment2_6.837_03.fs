module Assignment2_6_837

open OpenCvSharp
open System
open Engine.Core.Color
open Engine.Core.Ray
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Assignment_6_837.Obj
//open Engine.Model.Obj
open FParsec


let keyword_OrthographicCamera : Parser<_, unit> = pstring "OrthographicCamera"
let keyword_Background : Parser<_, unit> = pstring "Background"
let keyword_Materials : Parser<_, unit> = pstring "Materials"
let keyword_Material : Parser<_, unit> = pstring "Material"
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
let keyword_AmbientLight : Parser<_, unit> = pstring "ambientLight"
let keyword_Lights : Parser<_, unit> = pstring "Lights"
let keyword_NumLights : Parser<_, unit> = pstring "numLights"
let keyword_DirectionalLight : Parser<_, unit> = pstring "DirectionalLight"
let keyword_PerspectiveCamera : Parser<_, unit> = pstring "PerspectiveCamera"
let keyword_Angle : Parser<_, unit> = pstring "angle"
let keyword_Plane : Parser<_, unit> = pstring "Plane"
let keyword_Normal : Parser<_, unit> = pstring "normal"
let keyword_Offset : Parser<_, unit> = pstring "offset"
let keyword_Triangle : Parser<_, unit> = pstring "Triangle"
let keyword_Vertex0 : Parser<_, unit> = pstring "vertex0"
let keyword_Vertex1 : Parser<_, unit> = pstring "vertex1"
let keyword_Vertex2 : Parser<_, unit> = pstring "vertex2"
let keyword_TriangleMesh : Parser<_, unit> = pstring "TriangleMesh"
let keyword_Objfile : Parser<_, unit> = pstring "obj_file"
let keyword_Transform : Parser<_, unit> = pstring "Transform"
let keyword_UniformScale : Parser<_, unit> = pstring "UniformScale"
let keyword_Translate : Parser<_, unit> = pstring "Translate"
let keyword_Scale : Parser<_, unit> = pstring "Scale"
let keyword_XRotate : Parser<_, unit> = pstring "XRotate"
let keyword_YRotate : Parser<_, unit> = pstring "YRotate"
let keyword_ZRotate : Parser<_, unit> = pstring "ZRotate"
let keyword_Rotate : Parser<_, unit> = pstring "Rotate"
let pfloat3 = many (spaces >>. pfloat .>> spaces)
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
type ICamera =
    abstract member GetRay: Point2D -> Ray

[<Struct>]
type Background =
    val bgColor : Color
    val ambientColor : Color
    new(b,a) = {bgColor=b;ambientColor=a;}
[<Struct>]
type PerspectiveCamera =
    val center : Point
    val direction : Vector
    val horizontal : Vector
    val up : Vector
    val aspect : float  // width / height
    val fov : float
    val leftBottom : Point
    new(c,d:Vector, u:Vector, f, asp) =
        let dir = d.Normalize
        let hori = dir.Cross(u.Normalize)
        let up = hori.Cross(dir)
        let up_size = tan(f * Math.PI / 360.) * 2.0
        let hori_size = up_size * asp
        {
            center = c;
            direction = dir;
            up = up * up_size;
            horizontal = hori * hori_size;
            aspect = asp;
            fov = f;
            leftBottom = c + dir - (up * up_size) / 2.0 - (hori * hori_size) / 2.0;
        }
    member this.GetRay(p:Point2D) =
        let pos = this.leftBottom + p.x * this.horizontal + (1.0 - p.y) * this.up
        Ray(this.center, (pos - this.center).Normalize)
    interface ICamera with
        member this.GetRay(p) = this.GetRay(p)
[<Struct>]
type OrthographicCamera =
    val center : Point
    val direction : Vector
    val horizontal : Vector
    val up : Vector
    val size : float
    val leftBottom : Point
    new(c, d:Vector, u:Vector, size) =
        let dir = d.Normalize
        let hori = dir.Cross(u.Normalize)
        let up = hori.Cross(dir)
        {
            center = c;
            direction = dir;
            up = size * up;
            horizontal = size * hori;
            size = size;
            leftBottom = c - size * up / 2. - size * hori / 2.;
        }
    member this.GetRay(p:Point2D) =
        let pos = this.leftBottom + p.x * this.horizontal + (1.0 - p.y) * this.up
        Ray(pos, this.direction)
    interface ICamera with
        member this.GetRay(p) = this.GetRay(p)
        
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
    val normal : Vector
    val ray: Ray
    new(t,r,mat,p,nm) = {hit=true;t = t;material=mat;normal=nm;ray=r;point=p;}
type IObject3D =
    abstract member Intersect : r:Ray * tMin:float * tMax:float -> Hit
type ILight =
    abstract member GetIllumination : p:Point -> toLight:Vector * Color
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
                    Hit(tmp,r,this.materialIdx, p, (p-this.center)/this.radius)
                else
                    let tmp = (-b + sqrt(discriminant))/(2.0*a)
                    if tmp < tMax && tmp > tMin then
                        let p = r.PointAtParameter(tmp)
                        Hit(tmp,r,this.materialIdx, p, (p-this.center)/this.radius)
                    else
                        Hit()
            else
                Hit()
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end
type Plane =
    struct
        val normal : Vector
        val distance : float
        val point : Point
        val material : int
        new(nm:Vector, dist, mat) =
            let _nm = nm.Normalize
            {
                normal=_nm;
                distance=dist;
                point=Point()+_nm*dist;
                material=mat;
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            let denom = this.normal.Dot(r.Direction())
            //if denom < - 1e-6 then
            if abs denom > 1e-6 then
                let v = this.point-r.Origin()
                let t = v.Dot(this.normal) / denom
                if t > tMin then
                    let pos = r.PointAtParameter(t)
                    Hit(t, r, this.material, pos, this.normal)
                else
                    Hit()
            else
                Hit()
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end
type Triangle =
    struct
        val v0 : Point
        val v1 : Point
        val v2 : Point
        val normal : Vector
        val material : int
        new(_v0:Point, _v1:Point, _v2:Point, mat) =
            let e1 = _v1 - _v0
            let e2 = _v2 - _v0
            let nm = e1.Cross(e2).Normalize
            {
                v0 = _v0; v1 =_v1; v2 = _v2;
                normal = nm; material = mat;
            }
        member this.PreCalcu(ray:Ray) =
            let v0 = this.v0
            let v1 = this.v1
            let v2 = this.v2
            let e1 = v1-v0
            let e2 = v2-v0
            let s1 = ray.Direction().Cross(e2)
            let divisor = s1.Dot(e1)
            if divisor < 1e-6 then
                false, 0.,0.,0.
            else
                let invDivisor = 1./divisor
                let d = ray.Origin() - v0
                let b1 = d.Dot(s1) * invDivisor
                if b1 <0. || b1 > 1. then
                    false, 0.,0., 0.
                else
                    let s2 = d.Cross(e1)
                    let b2 = ray.Direction().Dot(s2) * invDivisor
                    if b2 < 0. || b1 + b2 > 1. then
                        false, 0., 0., 0.
                    else
                        let t = e2.Dot(s2) * invDivisor
                        true, t, b1, b2
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            let bHit, t, beta, gamma = this.PreCalcu(r)
            if bHit && t > tMin then
                let p = r.PointAtParameter(t)
                let alpha = 1. - beta - gamma
                assert(alpha >= 0. && alpha <= 1.)
                Hit(t, r, this.material, p, this.normal)
                //HitRecord(true, t, p, normal, r, Some this.material)
            else
                Hit()
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end

type TriangleMesh =
    struct
        val faces : Triangle array
        new(init:ObjIncludeInfo, material:int) =
            let idxs = init.Faces |> Array.map (fun (idx,_,_) -> idx)
            let arr = idxs |> Array.map (fun idx ->
                                Triangle(init.Vertexs[idx.i],init.Vertexs[idx.j],
                                         init.Vertexs[idx.k],material))
            {
                faces = arr;
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            this.faces |> Array.map (fun f -> f.Intersect(r,tMin,tMax)) |>
                Array.minBy (fun hit -> if hit.hit then hit.t else tMax)
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end

type Transform =
    struct
        val mats : Matrix4x4
        val invMats : Matrix4x4
        val normalMats : Matrix4x4
        val transformed : IObject3D
        val material : int
        new(matr:Matrix4x4, invMatr:Matrix4x4, obj:IObject3D, mate:int) =
            {
                mats=matr;
                invMats=invMatr;
                normalMats=invMatr.Transpose();
                transformed=obj;
                material=mate;
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            let newRay = Ray(r.Origin() * this.invMats, (r.Direction() * this.invMats).Normalize)
            let hit = this.transformed.Intersect(newRay, tMin, tMax)
            if hit.hit then
                let hitPoint = hit.point*this.mats
                let hitNormal = (hit.normal*this.normalMats).Normalize
                let len = (hitPoint - r.Origin()).Length
                Hit(len,r,hit.material,hitPoint, hitNormal)
            else
                hit
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end

type Group =
    struct
        val objects : IObject3D list
        new(l) = {objects = l;}
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            this.objects |> List.map(fun o -> o.Intersect(r,tMin,tMax)) |>
                List.minBy (fun h -> if h.hit then h.t else tMax)
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
    end

type DirectionalLight =
    struct
        val dir : Vector
        val color : Color
        new(d:Vector, c) = {dir=d.Normalize; color=c;}
        member this.GetIllumination(p:Point) =
            (-this.dir,this.color)
        interface ILight with
            member this.GetIllumination(p:Point) = this.GetIllumination(p)
    end

[<Struct>]
type Scene =
    val camera : ICamera
    val light : ILight list
    val background : Background
    val materials : AssetMaterial list
    val objs : IObject3D list
    new(cam,ls,bg,mats,objs) = {camera=cam;light=ls;background=bg;materials=mats;objs=objs;}
    member this.GetNormalDepth(p:Point2D) =
        let tmin = 0.000001
        let tmax = 9999999.
        let ray = this.camera.GetRay(p)
        let hit = this.objs |> List.map (fun item -> item.Intersect(ray, tmin, tmax)) |>
                    List.minBy(fun hit -> if hit.hit then hit.t else tmax)
        if hit.hit then
            Color(abs hit.normal.x, abs hit.normal.y, abs hit.normal.z), hit.t
        else
            Color(), 0.
    member this.GetColorDepth(p:Point2D) =
        let tmin = 0.000001
        let tmax = 9999999.
        let ray = this.camera.GetRay(p)
        let hit = this.objs |> List.map (fun item -> item.Intersect(ray, tmin, tmax)) |>
                    List.minBy(fun hit -> if hit.hit then hit.t else tmax)
        if hit.hit then
            let matIdx = hit.material
            let objColor = this.materials[matIdx].color
            let mutable finalColor = Color()
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)
                let d = toLight.Dot(hit.normal)
                finalColor <- finalColor +
                    if d > 0. then
                        d * c * objColor
                    else
                        Color()
            this.GetAmbientLight() * objColor + finalColor, hit.t
        else
            this.background.bgColor, 0.
    member this.GetLight() = this.light
    member this.GetAmbientLight() = this.background.ambientColor
let toOrthographicCamera (((c,dir),up),size) =
    OrthographicCamera(c,dir,up,size) :> ICamera
let pOrthographicCameraContent =
    keyword_Center .>> spaces >>. pPoint .>> spaces .>>
    keyword_Direction .>> spaces .>>. pVector .>>
    keyword_Up .>> spaces .>>. pVector .>>
    keyword_Size .>> spaces .>>. pfloat .>> spaces |>> toOrthographicCamera


let pNumLights =
    keyword_NumLights .>> spaces >>. pint32
let pDirectionalLight =
    keyword_DirectionalLight >>.
    between beginScope endScope
            (keyword_Direction .>> spaces >>. pVector .>> spaces .>> keyword_Color .>> spaces .>>. pColor |>> DirectionalLight)

let pMaterial =
    keyword_Material >>.
    between beginScope endScope
            (keyword_DiffuseColor .>>spaces >>. pColor |>> AssetMaterial)

let toIdxSphere (i, (c,r)) = Sphere(c,r,i)
let toIdxSpheres (i, l) = l |> List.map (fun (c,r) -> Sphere(c,r,i) :> IObject3D)
let toSphere (c, r) = Sphere(c,r,0)
let toBackground (b,a) = Background(b,a)
let toILight l = l :> ILight
let toScene ((((cam,l),c),mats),objs) =
    Scene(cam,l,c,mats,objs)


type TransformAst =
    | UniformScale_node of float
    | Scale_node of Vector
    | Translate_node of Vector
    | XRotate_node of float
    | YRotate_node of float
    | ZRotate_node of float
    | Rotate_node of Vector
type ObjectAST =
    | Sphere_node of Point * float
    | Plane_node of Vector * float
    | Triangle_node of (Point * Point) * Point
    | TrianbleMesh_node of string
    | Transform_node of TransformAst list * ObjectAST
    | Objects_node of int * (ObjectAST list)
    | Group_node of ObjectAST list

let rec toObject(obj:ObjectAST, material:int) : IObject3D =
    match obj with
    | Sphere_node (p,r) -> Sphere(p,r,material)
    | Plane_node (nm, off) -> Plane(nm, off, material)
    | Triangle_node ((v0,v1),v2) -> Triangle(v0,v1,v2,material)
    | TrianbleMesh_node filename ->
        let info = LoadModel(filename)
        TriangleMesh(info,material)
    | Transform_node (l,ast) ->
        let mats =
            l |> List.map(fun node ->
                            match node with
                            | UniformScale_node s -> Matrix4x4.MakeScaleMatrix(s,s,s)
                            | Scale_node s -> Matrix4x4.MakeScaleMatrix(s.x,s.y,s.z)
                            | Translate_node t -> Matrix4x4.MakeDisplacementMatrix(t.x,t.y,t.z)
                            | XRotate_node ang -> Matrix4x4.MakeRotationXMatrix(ang)
                            | YRotate_node ang -> Matrix4x4.MakeRotationYMatrix(ang)
                            | ZRotate_node ang -> Matrix4x4.MakeRotationZMatrix(ang)
                            | Rotate_node ang -> Matrix4x4.MakeRotationMatrix(ang.x,ang.y,ang.z))
            |> List.rev
            |> List.reduce (fun l r -> l * r)
        let invMats =
            l |> List.map(fun node ->
                            match node with
                            | UniformScale_node s -> Matrix4x4.MakeScaleInvMatrix(s,s,s)
                            | Scale_node s -> Matrix4x4.MakeScaleInvMatrix(s.x,s.y,s.z)
                            | Translate_node t -> Matrix4x4.MakeDisplacementInvMatrix(t.x,t.y,t.z)
                            | XRotate_node ang -> Matrix4x4.MakeRotationXInvMatrix(ang)
                            | YRotate_node ang -> Matrix4x4.MakeRotationYInvMatrix(ang)
                            | ZRotate_node ang -> Matrix4x4.MakeRotationZInvMatrix(ang)
                            | Rotate_node ang -> Matrix4x4.MakeRotationInvMatrix(ang.x,ang.y,ang.z))
            |> List.reduce (fun l r -> l * r)
        let obj = toObject(ast, material)
        Transform(mats,invMats,obj,material)
    // use self saved material
    | Objects_node (m, l) ->
        let ls = List.map (fun obj -> toObject(obj, m)) l
        Group(ls)
    | Group_node l ->
        let ls = List.map (fun obj -> toObject(obj, -1)) l
        Group(ls)
let groupToObjects(obj:ObjectAST) : IObject3D list =
    [toObject(obj, -1)]

let pUniformScale =
    keyword_UniformScale >>. spaces >>. pfloat .>> spaces |>> TransformAst.UniformScale_node
let pTranslate =
    keyword_Translate >>. spaces >>. pVector .>> spaces |>> TransformAst.Translate_node
let pScale =
    keyword_Scale >>. spaces >>. pVector .>> spaces |>> TransformAst.Scale_node
let pXRotate =
    keyword_XRotate >>. spaces >>. pfloat .>> spaces |>> TransformAst.XRotate_node
let pYRotate =
    keyword_YRotate >>. spaces >>. pfloat .>> spaces |>> TransformAst.YRotate_node
let pZRotate =
    keyword_ZRotate >>. spaces >>. pfloat .>> spaces |>> TransformAst.ZRotate_node
let pRotate =
    keyword_Rotate >>. spaces >>. pVector .>> spaces |>> TransformAst.Rotate_node

let pNumObjects =
    keyword_NumObjects .>> spaces >>. pint32 .>> spaces
let pNoIdxSphere =
    keyword_Sphere >>.
    between beginScope endScope
            (keyword_Center >>. spaces >>. pPoint .>> spaces .>> keyword_Radius .>> spaces .>>. pfloat)
let pIdxSphere =
    keyword_MaterialIndex >>. spaces >>. pint32 .>> spaces .>>.
    many pNoIdxSphere |>> toIdxSpheres
let pSpheres = many pIdxSphere |>> List.concat
let pSphere =
    keyword_Sphere >>.
    between beginScope endScope
            (keyword_Center >>. spaces >>. pPoint .>> spaces .>> keyword_Radius .>> spaces .>>. pfloat |>> ObjectAST.Sphere_node)
let pPlane =
    keyword_Plane >>.
    between beginScope endScope
            (keyword_Normal >>. spaces >>. pVector .>> spaces .>>
             keyword_Offset .>> spaces .>>. pfloat |>> ObjectAST.Plane_node)
let pTriangle =
    keyword_Triangle >>.
    between beginScope endScope
            (keyword_Vertex0 >>. spaces >>. pPoint .>> spaces .>>
             keyword_Vertex1 .>> spaces .>>. pPoint .>> spaces .>>
             keyword_Vertex2 .>> spaces .>>. pPoint |>> ObjectAST.Triangle_node)
let pFilename =
    manyChars (letter <|> digit <|> anyOf "._")
let pTriangleMesh =
    keyword_TriangleMesh >>.
    between beginScope endScope
            (keyword_Objfile >>. spaces >>. pFilename |>> ObjectAST.TrianbleMesh_node)
    
let pTransformation =
    many (choice [
        pUniformScale
        pTranslate
        pScale
        pXRotate
        pYRotate
        pZRotate
        pRotate
    ])
let pRecObjectAst, pRecObjectAstRef = createParserForwardedToRef<ObjectAST,unit>()
let pRecGroup, pRecGroupRef = createParserForwardedToRef<ObjectAST, unit>()
let pTransform =
    keyword_Transform >>.
    between beginScope endScope
            (pTransformation .>> spaces .>>. pRecObjectAst) |>> ObjectAST.Transform_node
//************ parser Group of Objects
let pObjectAst =
    choice [
        pSphere
        pPlane
        pTriangleMesh
        pTriangle
        pTransform
        pRecGroup
    ]
pRecObjectAstRef.Value <- pObjectAst
let pObject =
    keyword_MaterialIndex >>. spaces >>. pint32 .>> spaces .>>.
    many pObjectAst |>> ObjectAST.Objects_node // |>> toObjects
let pObjects = many (pObjectAst <|> pObject) |>> ObjectAST.Group_node // |>> List.concat
let pOneGroup =
    keyword_Group >>.
    between beginScope endScope
            (pNumObjects >>. pObjects)
pRecGroupRef.Value <- pOneGroup

let toPerspectiveCamera(((c,d),u),a) = PerspectiveCamera(c,d,u,a,1.) :> ICamera
let pPerspectiveCameraContent =
    keyword_Center >>. pPoint .>>
    keyword_Direction .>>. pVector .>>
    keyword_Up .>>. pVector .>>
    keyword_Angle .>> spaces .>>. pfloat .>> spaces |>> toPerspectiveCamera
let pPerspectiveCamera =
    keyword_PerspectiveCamera .>> spaces >>.
    between beginScope endScope
            pPerspectiveCameraContent

let pOrthographicCamera =
    keyword_OrthographicCamera >>.
    between beginScope endScope
            pOrthographicCameraContent

let pCamera =
    pPerspectiveCamera <|> pOrthographicCamera
let pLights =
    keyword_Lights .>> spaces >>.
    between beginScope endScope
            (pNumLights .>> spaces >>. many (pDirectionalLight |>> toILight))
let pBackground =
    keyword_Background >>.
    between beginScope endScope
            (keyword_Color .>> spaces >>. pColor .>> keyword_AmbientLight .>> spaces .>>. pColor |>> toBackground)
let pMaterials =
    keyword_Materials >>.
    between beginScope endScope
            (keyword_NumMaterials .>> spaces >>. pint32 .>> spaces >>.
                many pMaterial)
let pGroup =
    pOneGroup |>> groupToObjects
let pScene =
    spaces >>. pCamera .>>
    spaces .>>. pLights .>>
    spaces .>>. pBackground .>>
    spaces .>>. pMaterials .>>
    spaces .>>. pGroup |>> toScene


let ParseScene filename =
    let lines = System.IO.File.ReadAllText(filename)
    match run pScene lines with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg
        assert(false)
        Scene(OrthographicCamera(Point(),Vector(),Vector(),0),[],Background(), [], [])

let CommandIndex(cmds:string[], s:string) =
    let idx = Array.tryFindIndex (fun x -> x = s) cmds
    match idx with
    | Some i -> i
    | _ -> -1
[<Struct>]
type RayTracer_Info =
    val inputFile: string
    val screenSize : int
    val drawNormal : bool
    val drawDepth : bool
    val minDepth : float
    val maxDepth : float
    val depthField : float
    new(command:string) =
        let ls = command.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        let inputIdx = CommandIndex(ls, "-input")
        assert(inputIdx <> -1)
        let sizeIdx = CommandIndex(ls, "-size")
        assert(sizeIdx <> -1)
        let normalIdx = CommandIndex(ls, "-normals")
        let depthIdx = CommandIndex(ls, "-depth")
        let bHasDepth = depthIdx <> -1
        let _minDepth = if bHasDepth then float ls[depthIdx+1] else 0.0
        let _maxDepth = if bHasDepth then float ls[depthIdx+2] else 10000.0
        {
            inputFile = ls[inputIdx+1];
            screenSize = int ls[sizeIdx+1];
            drawNormal = normalIdx <> -1;
            drawDepth = bHasDepth;
            minDepth = _minDepth;
            maxDepth = _maxDepth;
            depthField = _maxDepth - _minDepth;
        }

let ShowMat(screen:Screen, desc:string) =
    let (w,h) = screen.Size()
    let mat = new Mat(Size(w, h), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    for y in 0..h-1 do
        for x in 0..w-1 do
            let c = screen.Pixel(x,y) * 255.
            let vec = Vec3b(byte c.b, byte c.g, byte c.r)
            indexer[y,x] <- vec
    Cv2.ImShow(desc, mat)
    
let LoadScene_Assignment2() =
    // Source file(eg. scene1_01.txt) from https://groups.csail.mit.edu/graphics/classes/6.837/F04/assignments/assignment2/
    //let info = RayTracer_Info("raytracer -input scene2_06_plane.txt -size 200 200 -output output2_06.tga -depth 7 20 depth2_06.tga -normals normals2_06.tga")
    //let info = RayTracer_Info("raytracer -input scene2_09_bunny_200.txt -size 200 200 -output output2_09.tga")
    //let info = RayTracer_Info("raytracer -input scene2_13_rotated_squashed_sphere.txt -size 200 200 -output output2_13.tga -normals normals2_13.tga")
    //let info = RayTracer_Info("raytracer -input scene2_14_axes_cube.txt -size 200 200 -output output2_14.tga")
    let info = RayTracer_Info("raytracer -input scene2_15_crazy_transforms.txt -size 200 200 -output output2_15.tga")
    //let info = RayTracer_Info("raytracer -input scene2_16_t_scale.txt -size 200 200 -output output2_16.tga -depth 2 7 depth2_16.tga")
    let screen_size = info.screenSize
    let scene = ParseScene info.inputFile
    let screen = Screen(screen_size, screen_size)
    let normal = Screen(screen_size, screen_size)
    let depth = Screen(screen_size, screen_size)

    // render begin
    for x in 0..screen_size-1 do
        for y in 0..screen_size-1 do
            let p = Point2D(float x / float screen_size, float y / float screen_size)
            let c,d = scene.GetColorDepth(p)
            screen[x,y] <- c,d
            if info.drawNormal then
                normal[x,y] <- scene.GetNormalDepth(p)
            if info.drawDepth then
                let f =
                    if d > 0. then
                        (1.0 - (min ((d-info.minDepth)/info.depthField) 1.0))
                    else
                        0.
                depth[x,y] <- Color(f,f,f),d
    //render end

    ShowMat(screen, "Colored image")
    if info.drawNormal then ShowMat(normal, "Normal image")
    if info.drawDepth then ShowMat(depth, "Depth image")
    Cv2.WaitKey() |> ignore