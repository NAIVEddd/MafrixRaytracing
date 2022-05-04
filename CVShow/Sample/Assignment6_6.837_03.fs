module Assignment6_6_837

open OpenCvSharp
open System
open System.Collections.Generic
//open FSharp.Collections
open Engine.Core.Color
open Engine.Core.Ray
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Core.Aggregate
open Assignment_6_837.Obj
//open Engine.Model.Obj
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
let keyword_ReflectiveColor : Parser<_, unit> = pstring "reflectiveColor"
let keyword_SpecularColor : Parser<_, unit> = pstring "specularColor"
let keyword_Exponent : Parser<_, unit> = pstring "exponent"
let keyword_TransparentColor : Parser<_, unit> = pstring "transparentColor"
let keyword_IndexOfRefraction : Parser<_, unit> = pstring "indexOfRefraction"
let keyword_PointLight : Parser<_, unit> = pstring "PointLight"
let keyword_Position : Parser<_, unit> = pstring "position"
let keyword_Attenuation : Parser<_, unit> = pstring "attenuation"
let keyword_Checkerboard : Parser<_, unit> = pstring "Checkerboard"
let keyword_materialIndex : Parser<_, unit> = pstring "materialIndex"
let keyword_Noise : Parser<_, unit> = pstring "Noise"
let keyword_Octaves : Parser<_, unit> = pstring "octaves"
let keyword_Marble : Parser<_, unit> = pstring "Marble"
let keyword_Frequency : Parser<_, unit> = pstring "frequency"
let keyword_Amplitude : Parser<_, unit> = pstring "amplitude"
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

let RIGHT = 0
let LEFT = 1
let MIDDLE = 2
type AABB(a:Point, b:Point) =
    let pmin = a
    let pmax = b
    member this.min = pmin
    member this.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        // Code rewrite from:
        // https://github.com/erich666/GraphicsGems/blob/master/gems/RayBox.c
        let mutable inside = true
        let quadrant = Array.zeroCreate 3
        let candidatePlane = Array.zeroCreate<float> 3
        let origin = r.Origin()
        let dir = r.Direction()
        for i in 0..2 do
            if origin[i] < this.min[i] then
                quadrant[i] <- LEFT
                candidatePlane[i] <- this.min[i]
                inside <- false
            elif origin[i] > this.max[i] then
                quadrant[i] <- RIGHT
                candidatePlane[i] <- this.max[i]
                inside <- false
            else
                quadrant[i] <- MIDDLE
        if inside then
            true
        else
            let maxT = Array.zeroCreate<float> 3
            for i in 0..2 do
                if quadrant[i] <> MIDDLE && abs(dir[i]) > 1e-6 then
                    maxT[i] <- (candidatePlane[i]-origin[i])/dir[i]
                else
                    maxT[i] <- -1.

            let mutable whichPlane = 0
            for i in 0..2 do
                if maxT[whichPlane] < maxT[i] then
                    whichPlane <- i
            if maxT[whichPlane] < 0. then
                false
            else
                let mutable result = true
                for i in 0..2 do
                    if whichPlane <> i then
                        let coord = origin[i] + maxT[whichPlane] * dir[i]
                        if coord < this.min[i] || coord > this.max[i] then
                            result <- false
                result
//type AABB(a:Point, b:Point) =
//    let pmin = a
//    let pmax = b
//    member this.min = pmin
//    member this.max = pmax
//    member this.hit(r:Ray, tMin:float, tMax:float) =
//        let mutable bHit = true
//        for i in 0..2 do
//            if abs (r.Direction()[i]) > 1e-6 then
//                let invD = 1.0/r.Direction()[i]
//                let mutable t0 = ((pmin[i] - r.Origin()[i])*invD)
//                let mutable t1 = ((pmax[i] - r.Origin()[i])*invD)
//                if invD < 0. then
//                    let tmp = t0
//                    t0 <- t1
//                    t1 <- tmp
//                let tmin = if t0 >= tMin then t0 else tMin
//                let tmax = if t1 < tMax then t1 else tMax
//                bHit <- bHit && tmin < tmax
//        bHit
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

type PhongMaterial =
    new(diffuse,specular,exp,reflective,transparent,idxOfRefraction) = {}
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
    abstract member GetBoundingBox : unit -> Bound
type ILight =
    abstract member GetIllumination : p:Point -> toLight:Vector * Color

    
type IMaterial =
    abstract member NoLightColor : Hit -> Color
    abstract member Color : Hit * toLight: Vector -> Color
    abstract member Reflective : Hit -> bool * Vector * Color
    abstract member Transmitive : Hit -> bool * Vector * Color

type MaterialManager() =
    [<DefaultValue>]
    val mutable materials : IMaterial[]
    static let Manager =
        MaterialManager()
    static member GetManager() = Manager
    member this.Add(mat:IMaterial) =
        if isNull this.materials then
            this.materials <- [|mat|]
        else
            this.materials <- Array.insertAt this.materials.Length mat this.materials
    member this.Add(mats:IMaterial seq) =
        if isNull this.materials then
            this.materials <- Array.ofSeq mats
        else
            this.materials <- Array.insertManyAt this.materials.Length mats this.materials
    member this.Item
        with get(idx) = this.materials[idx]


[<Struct>]
type AssetMaterial =
    val color : Color
    val isReflective : bool
    val isTransmitted : bool
    val isSpecular : bool
    val exponent : float
    val reflectiveColor : Color
    val transmittedColor : Color
    val specularColor : Color
    val indexOfRefraction : float
    new(c) =
        {
            color = c;
            isReflective = false;
            isTransmitted = false;
            isSpecular = false;
            exponent = 0.;
            reflectiveColor = Color();
            transmittedColor = Color();
            specularColor = Color();
            indexOfRefraction = 1.;
        }
    new(c,reflective) =
        {
            color = c;
            isReflective = true;
            isTransmitted = false;
            isSpecular = false;
            exponent = 0.;
            reflectiveColor = reflective;
            transmittedColor = Color();
            specularColor = Color();
            indexOfRefraction = 1.;
        }
    new(diffu, specu, expon) =
        AssetMaterial(diffu, specu, expon, Color(), Color(), 1.)
    new(diffu, trans, reflec, idx) =
        {
            color = diffu;
            isReflective = reflec <> Color(0,0,0,1);
            isTransmitted = trans <> Color(0,0,0,1);
            isSpecular = false;
            exponent = 0.;
            reflectiveColor = reflec;
            transmittedColor = trans;
            specularColor = Color();
            indexOfRefraction = idx;
        }
    new(diffu, spec, exp, trans, reflec, idx) =
        {
            color = diffu;
            isReflective = reflec <> Color(0,0,0,1);
            isTransmitted = trans <> Color(0,0,0,1);
            isSpecular = spec <> Color(0,0,0,1);
            exponent = exp;
            reflectiveColor = reflec;
            transmittedColor = trans;
            specularColor = spec;
            indexOfRefraction = idx;
        }
    //member this.Color() =
    //    if this.isSpecular then
    //        this.specularColor
    member this.IsReflective() = this.isReflective
    member this.ReflectiveColor() = this.reflectiveColor
    member this.MirrorDirection(normal:Vector, incoming:Vector) =
        -2. * normal.Dot(incoming) * normal + incoming
    member this.IsTransmitted() = this.isTransmitted
    member this.TransmittedColor() = this.transmittedColor
    member this.TransmittedDirection(_normal:Vector, incoming:Vector) =
        let n1, n2, normal =
            if _normal.Dot(incoming) <0. then
                1.0, this.indexOfRefraction, _normal
            else    // inside the object
                this.indexOfRefraction, 1.0, -_normal
        let ratio = n1/n2
        let h = normal.Cross(incoming)
        let quart = 1.-ratio*ratio * h.LengthSquare
        if quart < 0. then  // all reflective
            this.MirrorDirection(normal, incoming)
        else
            ratio * normal.Cross((-normal).Cross(incoming)) - normal * sqrt(quart)

    interface IMaterial with
        member this.NoLightColor(hit:Hit) = this.color
        member this.Color(hit:Hit, toLight: Vector) =
            let d = max 0. (hit.normal.Dot(toLight))
            let baseColor = d * this.color

            if this.isSpecular then
                let q = max 0.0 (toLight.Dot(hit.ray.Direction()))
                let specu = this.specularColor * Math.Pow(q, this.exponent)
                baseColor + specu
            else
                baseColor
        member this.Reflective(hit:Hit) =
            if this.isReflective then
                true,this.MirrorDirection(hit.normal,hit.ray.Direction()), this.ReflectiveColor()
            else
                false,Vector(),Color()
        member this.Transmitive(hit:Hit) =
            if this.isTransmitted then
                true,this.TransmittedDirection(hit.normal,hit.ray.Direction()), this.transmittedColor
            else
                false,Vector(),Color()

type Sphere =
    struct
        val center: Point
        val radius: float
        val materialIdx: int
        val boundingBox : Bound
        new(c:Point, r:float, mate) =
            let vec = Vector(r,r,r)
            {
                center = c;
                radius = r;
                materialIdx = mate;
                boundingBox = Bound(c-vec, c+vec);
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            let oc = r.Origin() - this.center
            assert(1. - r.Direction().Length < 1e-6)
            let a = 1.  //
            let b = 2.0 * oc.Dot(r.Direction())
            let c = oc.Dot(oc) - this.radius*this.radius
            let discriminant = b*b-4.0*a*c
            if discriminant > 0 then
                let rootDiscrim = sqrt(discriminant)
                let q = if b < 0. then -0.5 * (b - rootDiscrim) else -0.5 * (b + rootDiscrim)
                let t0 = q // let t0 = q/a if a <> 1.0
                let t1 = c / q
                let tmin,tmax = min t0 t1, max t0 t1
                if tmin >= tMin && tmin < tMax then
                    let p = r.PointAtParameter(tmin)
                    Hit(tmin,r,this.materialIdx,p,(p-this.center).Normalize)
                elif tmax > tMin && tmax < tMax then
                    let p = r.PointAtParameter(tmax)
                    Hit(tmax,r,this.materialIdx,p,(p-this.center).Normalize)
                else
                    Hit()
                    
                //let rootDiscrim = sqrt(discriminant)
                //let tmp = (-b - rootDiscrim)/(2.0*a)
                //if tmp < tMax && tmp >= tMin then
                //    let p = r.PointAtParameter(tmp)
                //    Hit(tmp,r,this.materialIdx, p, (p-this.center)/this.radius)
                //else
                //    let tmp = (-b + rootDiscrim)/(2.0*a)
                //    if tmp < tMax && tmp >= tMin then
                //        let p = r.PointAtParameter(tmp)
                //        Hit(tmp,r,this.materialIdx, p, (p-this.center)/this.radius)
                //    else
                //        Hit()
            else
                Hit()
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end
type Plane =
    struct
        val normal : Vector
        val distance : float
        val point : Point
        val material : int
        val boundingBox : Bound
        new(nm:Vector, dist, mat) =
            let _nm = nm.Normalize
            let point = Point()+_nm*dist
            {
                normal=_nm;
                distance=dist;
                point=point;
                material=mat;
                boundingBox = Bound(point,point);
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
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end
type Triangle =
    struct
        val v0 : Point
        val v1 : Point
        val v2 : Point
        val normal : Vector
        val material : int
        val boundingBox : Bound
        new(_v0:Point, _v1:Point, _v2:Point, mat) =
            let e1 = _v1 - _v0
            let e2 = _v2 - _v0
            let nm = e1.Cross(e2).Normalize
            let b0 = Bound(_v0,_v1)
            let b1 = Bound.Union(b0,_v2)
            {
                v0 = _v0; v1 =_v1; v2 = _v2;
                normal = nm; material = mat;
                boundingBox = b1;
            }
        member this.PreCalcu(ray:Ray) =
            let v0 = this.v0
            let v1 = this.v1
            let v2 = this.v2
            let e1 = v1-v0
            let e2 = v2-v0
            let s1 = ray.Direction().Cross(e2)
            let divisor = s1.Dot(e1)
            
            //if divisor < 1e-6 then    // only front face
            if abs divisor < 1e-6 then  // with back face
                false, 0.,0.,0.
            else
                let invDivisor = 1./divisor
                let d = ray.Origin() - v0
                let b1 = d.Dot(s1) * invDivisor
                if (b1 < 0.) || (b1 > 1.) then
                    false, 0.,0., 0.
                else
                    let s2 = d.Cross(e1)
                    let b2 = ray.Direction().Dot(s2) * invDivisor
                    if (b2 < 0.) || ((b1 + b2) >= 1.) then
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
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end

type TriangleMesh =
    struct
        val faces : Triangle array
        val boundingBox : Bound
        new(init:ObjIncludeInfo, material:int) =
            let idxs = init.Faces |> Array.map (fun (idx,_,_) -> idx)
            let arr = idxs |> Array.map (fun idx ->
                                Triangle(init.Vertexs[idx.i],init.Vertexs[idx.j],
                                         init.Vertexs[idx.k],material))
            assert(arr.Length > 0)
            let bound =
                arr |> Array.map (fun f -> f.GetBoundingBox()) |> Array.reduce (fun l r -> Bound.Union(l,r))
            {
                faces = arr;
                boundingBox = bound;
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            this.faces |> Array.map (fun f -> f.Intersect(r,tMin,tMax)) |>
                Array.minBy (fun hit -> if hit.hit then hit.t else tMax)
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end

type Transform =
    struct
        val mats : Matrix4x4
        val invMats : Matrix4x4
        val normalMats : Matrix4x4
        val transformed : IObject3D
        val material : int
        val boundingBox : Bound
        new(matr:Matrix4x4, invMatr:Matrix4x4, obj:IObject3D, mate:int) =
            let rawBound = obj.GetBoundingBox()
            let bound = [|0..7|] |> Array.map(fun i -> rawBound.Corner(i) * matr) |>
                            Array.map(fun p -> Bound(p,p)) |>
                            Array.reduce(fun l r -> Bound.Union(l, r))
            {
                mats=matr;
                invMats=invMatr;
                normalMats=invMatr.Transpose();
                transformed=obj;
                material=mate;
                boundingBox = bound;
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
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end


[<Struct>]
type CheckerBoard =
    val transform : Transform
    val materialIndex1 : int
    val materialIndex2 : int
    // index : int[]
    new(trans, mat1, mat2) =
        {
            transform=trans;
            materialIndex1=mat1;
            materialIndex2=mat2;
        }
    member private this.GetMaterial(p:Point) =
        let newP = p * this.transform.mats
        let x1 = int(floor newP.x) % 2 <> 0  // is odd
        let y1 = int(floor newP.y) % 2 <> 0
        let z1 = int(floor newP.z) % 2 <> 0
        if x1 then
            if y1 then
                if z1 then
                    MaterialManager.GetManager()[0]
                else
                    MaterialManager.GetManager()[1]
            else
                if z1 then
                    MaterialManager.GetManager()[1]
                else
                    MaterialManager.GetManager()[0]
        else
            if y1 then
                if z1 then
                    MaterialManager.GetManager()[1]
                else
                    MaterialManager.GetManager()[0]
            else
                if z1 then
                    MaterialManager.GetManager()[0]
                else
                    MaterialManager.GetManager()[1]
    interface IMaterial with
        member this.NoLightColor(hit:Hit) = this.GetMaterial(hit.point).NoLightColor(hit)
        member this.Color(hit:Hit, toLight: Vector) =
            this.GetMaterial(hit.point).Color(hit,toLight)
        member this.Reflective(hit:Hit) =
            this.GetMaterial(hit.point).Reflective(hit)
        member this.Transmitive(hit:Hit) =
            this.GetMaterial(hit.point).Transmitive(hit)

type PerlinNoise() =
    static let permutation =
        [| 151;160;137;91;90;15;
           131;13;201;95;96;53;194;233;7;225;140;36;103;30;69;142;8;99;37;240;21;10;23;
           190; 6;148;247;120;234;75;0;26;197;62;94;252;219;203;117;35;11;32;57;177;33;
           88;237;149;56;87;174;20;125;136;171;168; 68;175;74;165;71;134;139;48;27;166;
           77;146;158;231;83;111;229;122;60;211;133;230;220;105;92;41;55;46;245;40;244;
           102;143;54; 65;25;63;161; 1;216;80;73;209;76;132;187;208; 89;18;169;200;196;
           135;130;116;188;159;86;164;100;109;198;173;186; 3;64;52;217;226;250;124;123;
           5;202;38;147;118;126;255;82;85;212;207;206;59;227;47;16;58;17;182;189;28;42;
           223;183;170;213;119;248;152; 2;44;154;163; 70;221;153;101;155;167; 43;172;9;
           129;22;39;253; 19;98;108;110;79;113;224;232;178;185; 112;104;218;246;97;228;
           251;34;242;193;238;210;144;12;191;179;162;241; 81;51;145;235;249;14;239;107;
           49;192;214; 31;181;199;106;157;184; 84;204;176;115;121;50;45;127; 4;150;254;
           138;236;205;93;222;114;67;29;24;72;243;141;128;195;78;66;215;61;156;180|]
    static let p = Array.init 512 (fun i -> PerlinNoise.GetPermutation(i%256))
    static member GetP(i) = p[i]
    static member GetPermutation(index) = permutation[index]
    static member fade(t:float) = t*t*t*(t*(t*6.-15.)+10.)
    static member lerp(t:float,a:float,b:float) = a + t * (b-a)
    static member grad(hash:int,x:float,y:float,z:float) =
        let h = hash &&& 15
        let u = if h < 8 then x else y
        let v = if h < 4 then y elif (h = 12 || h = 14) then x else z
        let _u = if h &&& 1 = 0 then u else -u
        let _v = if h &&& 2 = 0 then v else -v
        _u + _v
    static member noise(p:Point) =
        let X = int(floor p.x) &&& 255
        let Y = int(floor p.y) &&& 255
        let Z = int(floor p.z) &&& 255
        let _x = p.x - floor p.x
        let _y = p.y - floor p.y
        let _z = p.z - floor p.z
        let u = PerlinNoise.fade(_x)
        let v = PerlinNoise.fade(_y)
        let w = PerlinNoise.fade(_z)
        let A = PerlinNoise.GetP(X)+Y
        let AA = PerlinNoise.GetP(A)+Z
        let AB = PerlinNoise.GetP(A+1)+Z
        let B = PerlinNoise.GetP(X+1)+Y
        let BA = PerlinNoise.GetP(B)+Z
        let BB = PerlinNoise.GetP(B+1)+Z
        PerlinNoise.lerp(w,
            PerlinNoise.lerp(v,
                PerlinNoise.lerp(u, PerlinNoise.grad(PerlinNoise.GetP(AA), _x, _y, _z),
                                    PerlinNoise.grad(PerlinNoise.GetP(BA), _x-1., _y, _z)),
                PerlinNoise.lerp(u, PerlinNoise.grad(PerlinNoise.GetP(AB), _x, _y-1., _z),
                                    PerlinNoise.grad(PerlinNoise.GetP(BB), _x-1., _y-1., _z))),
            PerlinNoise.lerp(v,
                PerlinNoise.lerp(u, PerlinNoise.grad(PerlinNoise.GetP(AA+1), _x, _y, _z-1.),
                                    PerlinNoise.grad(PerlinNoise.GetP(BA+1), _x-1., _y, _z-1.)),
                PerlinNoise.lerp(u, PerlinNoise.grad(PerlinNoise.GetP(AB+1), _x, _y-1., _z-1.),
                                    PerlinNoise.grad(PerlinNoise.GetP(BB+1), _x-1., _y-1., _z-1.))))

let LerpColor(t:float, c1:Color, c2:Color) =
    Color(PerlinNoise.lerp(t,c1.r,c2.r),
          PerlinNoise.lerp(t,c1.g,c2.g),
          PerlinNoise.lerp(t,c1.b,c2.b))
[<Struct>]
type Noise =
    val transform : Transform
    val materialIdx1 : int
    val materialIdx2 : int
    val octaves : int
    new(trans, id1, id2, octav) =
        {
            transform=trans;
            materialIdx1=id1;
            materialIdx2=id2;
            octaves = octav;
        }

    member this.Calc(hit:Hit) =
        let p = hit.point
        let mutable f = PerlinNoise.noise(p)
        let mutable m = 1.
        for i in 1..this.octaves do
            m <- m * 2.
            let np = Point(m*p.x,m*p.y,m*p.z)
            f <- PerlinNoise.noise(np) / m
        f
    member this.NoLightColor(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        let material1=MaterialManager.GetManager()[this.materialIdx1];
        let material2=MaterialManager.GetManager()[this.materialIdx2];
        let color1 = material1.NoLightColor(newHit)
        let color2 = material2.NoLightColor(newHit)
        LerpColor(t,color1,color2)
        //let t = this.Calc(newHit)
        //if t > 0.5 then
        //    let material1=MaterialManager.GetManager()[this.materialIdx1];
        //    material1.NoLightColor(hit)
        //else
        //    let material2=MaterialManager.GetManager()[this.materialIdx2];
        //    material2.NoLightColor(hit)
    member this.Color(hit:Hit, toLight:Vector) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        let material1=MaterialManager.GetManager()[this.materialIdx1];
        let material2=MaterialManager.GetManager()[this.materialIdx2];
        let color1 = material1.Color(newHit,toLight)
        let color2 = material2.Color(newHit,toLight)
        LerpColor(t,color1,color2)
        //let t = this.Calc(newHit)
        //if t > 0.5 then
        //    let material1=MaterialManager.GetManager()[this.materialIdx1];
        //    material1.Color(hit,toLight)
        //else
        //    let material2=MaterialManager.GetManager()[this.materialIdx2];
        //    material2.Color(hit,toLight)
    member this.Reflective(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let material1=MaterialManager.GetManager()[this.materialIdx1];
        let material2=MaterialManager.GetManager()[this.materialIdx2];
        let ref1, dir1, color1 = material1.Reflective(newHit)
        let ref2, dir2, color2 = material2.Reflective(newHit)
        if ref1 && ref2 then
            let t = this.Calc(newHit)
            if t > 0.5 then
                ref1, dir1, color1
            else
                ref2, dir2, color2
            //ref1, dir1, LerpColor(t, color1, color2)
        else
            ref1, dir1, color1
        //let t = this.Calc(newHit)
        //if t > 0.5 then
        //    let material1=MaterialManager.GetManager()[this.materialIdx1];
        //    material1.Reflective(hit)
        //else
        //    let material2=MaterialManager.GetManager()[this.materialIdx2];
        //    material2.Reflective(hit)
    member this.Transmitive(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let material1=MaterialManager.GetManager()[this.materialIdx1];
        let material2=MaterialManager.GetManager()[this.materialIdx2];
        let trans1, dir1, color1 = material1.Transmitive(newHit)
        let trans2, dir2, color2 = material2.Transmitive(newHit)
        if trans1 || trans2 then
            let t = this.Calc(newHit)
            trans1, dir1, LerpColor(t, color1, color2)
        else
            trans1, dir1, color1
        //let t = this.Calc(newHit)
        //if t > 0.5 then
        //    let material1=MaterialManager.GetManager()[this.materialIdx1];
        //    material1.Transmitive(hit)
        //else
        //    let material2=MaterialManager.GetManager()[this.materialIdx2];
        //    material2.Transmitive(hit)

    interface IMaterial with
        member this.NoLightColor(hit:Hit) = this.NoLightColor(hit)
        member this.Color(hit:Hit, toLight: Vector) = this.Color(hit,toLight)
        member this.Reflective(hit:Hit) = this.Reflective(hit)
        member this.Transmitive(hit:Hit) = this.Transmitive(hit)

[<Struct>]
type Marble =
    val transform : Transform
    val materialIdx1 : int
    val materialIdx2 : int
    val octaves : int
    val frequency : float
    val amplitude : float
    new(trans, id1, id2, octaves, freq, ampl) =
        {
            transform=trans;
            materialIdx1=id1;
            materialIdx2=id2;
            octaves=octaves;
            frequency=freq;
            amplitude=ampl;
        }

    member this.Calc(hit:Hit) =
        let p = hit.point
        let mutable f = PerlinNoise.noise(p)
        //let mutable m = 1.
        //for i in 1..this.octaves do
        //    m <- m * 2.
        //    let np = Point(m*p.x,m*p.y,m*p.z)
        //    f <- PerlinNoise.noise(np) / m
        sin(this.frequency * p.x + this.amplitude * f)
    member this.NoLightColor(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        //LerpColor(t,color1,color2)
        if t > 0.5 then
            let material1=MaterialManager.GetManager()[this.materialIdx1];
            material1.NoLightColor(newHit)
        else
            let material2=MaterialManager.GetManager()[this.materialIdx2];
            material2.NoLightColor(newHit)
    member this.Color(hit:Hit, toLight:Vector) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        if t > 0.5 then
            let material1=MaterialManager.GetManager()[this.materialIdx1];
            material1.Color(newHit,toLight)
        else
            let material2=MaterialManager.GetManager()[this.materialIdx2];
            material2.Color(newHit,toLight)
        //LerpColor(t,color1,color2)
    member this.Reflective(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        if t > 0.5 then
            let material1=MaterialManager.GetManager()[this.materialIdx1];
            material1.Reflective(newHit)
        else
            let material2=MaterialManager.GetManager()[this.materialIdx2];
            material2.Reflective(newHit)
    member this.Transmitive(hit:Hit) =
        let newHit = Hit(hit.t, hit.ray, hit.material, hit.point * this.transform.mats, hit.normal)
        let t = this.Calc(newHit)
        if t > 0.5 then
            let material1=MaterialManager.GetManager()[this.materialIdx1];
            material1.Transmitive(newHit)
        else
            let material2=MaterialManager.GetManager()[this.materialIdx2];
            material2.Transmitive(newHit)

    interface IMaterial with
        member this.NoLightColor(hit:Hit) = this.NoLightColor(hit)
        member this.Color(hit:Hit, toLight: Vector) = this.Color(hit,toLight)
        member this.Reflective(hit:Hit) = this.Reflective(hit)
        member this.Transmitive(hit:Hit) = this.Transmitive(hit)
type Group =
    struct
        val objects : IObject3D list
        val boundingBox : Bound
        new(l) =
            let bound = l |> List.map(fun (o: IObject3D) -> o.GetBoundingBox()) |>
                             List.reduce(fun l r -> Bound.Union(l,r))
            {
                objects = l;
                boundingBox = bound;
            }
        member this.Intersect(r:Ray, tMin:float, tMax:float) =
            this.objects |> List.map(fun o -> o.Intersect(r,tMin,tMax)) |>
                List.minBy (fun h -> if h.hit then h.t else tMax)
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end

let BoundToTriangle(boundingBox : Bound)  :IObject3D[]=
    let points = [|0..7|] |> Array.map(fun i -> boundingBox.Corner(i))
    let trianglesIdx =
        [|(0,1,3);(0,3,2);  // back
          (1,5,7);(1,7,3);  // right
          (5,4,6);(5,6,7);  // front
          (4,0,2);(4,2,6);  // left
          (4,5,1);(4,1,0);  // bottom
          (2,3,7);(2,7,6);  // top
        |]
    trianglesIdx |> Array.map (fun (i,j,k) -> Triangle(points[i],points[j],points[k],-1))
    
let numTail x =
    if x >= 0. then
        x - floor x
    else
        x - ceil x

type MarchingInfo =
    val i : int
    val j : int
    val k : int
    val t : float
    val ray : Ray
    val cellSize : Vector
    val d_t : Vector
    val next : Vector
    val sign_x : int
    val sign_y : int
    val sign_z : int
    val normal : Vector
    new(i:int,j:int,k:int,t:float,dt:Vector,next:Vector,r:Ray,cell:Vector,nm:Vector) =
        let dir = r.Direction()
        let s_x = if dir.x > 0. then 1 else -1
        let s_y = if dir.y > 0. then 1 else -1
        let s_z = if dir.z > 0. then 1 else -1

        {
            t=t;
            i=i;j=j;k=k;d_t=dt;next=next;
            normal=nm;
            cellSize=cell;
            ray=r;
            sign_x=s_x;sign_y=s_y;sign_z=s_z;
        }
    static member Init(hit:Hit,bound:Bound,gridNum:Vector,cell:Vector) =
        let pos = hit.point
        let dir = hit.ray.Direction()
        let normal = hit.normal
        let (_i,_j,_k) = MarchingInfo.GetGridIndex(pos, bound, gridNum)
        let i = if _i = (int gridNum.x) && (dir.x < 0.) then _i - 1 else _i
        let j = if _j = (int gridNum.y) && (dir.y < 0.) then _j - 1 else _j
        let k = if _k = (int gridNum.z) && (dir.z < 0.) then _k - 1 else _k
        let dt_x = if (abs dir.x) > 1e-6 then abs (cell.x/dir.x) else System.Double.PositiveInfinity
        let dt_y = if (abs dir.y) > 1e-6 then abs (cell.y/dir.y) else System.Double.PositiveInfinity
        let dt_z = if (abs dir.z) > 1e-6 then abs (cell.z/dir.z) else System.Double.PositiveInfinity
        let dt = Vector(dt_x,dt_y,dt_z)
        let ori_grid = pos - bound.pMin
        let _next_x =
            if dir.x >= 0. then
                (1. - numTail (ori_grid.x/cell.x)) * dt_x
            else
                (numTail (ori_grid.x/cell.x)) * dt_x
        let _next_y =
            if dir.y >= 0. then
                (1. - numTail (ori_grid.y/cell.y)) * dt_y
            else
                (numTail (ori_grid.y/cell.y)) * dt_y
        let _next_z =
            if dir.z >= 0. then
                (1. - numTail (ori_grid.z/cell.z)) * dt_z
            else
                (numTail (ori_grid.z/cell.z)) * dt_z
        let next_x = if _next_x < 1e-6 then dt_x else _next_x
        let next_y = if _next_y < 1e-6 then dt_y else _next_y
        let next_z = if _next_z < 1e-6 then dt_z else _next_z
        assert(next_x > 0.)
        assert(next_y > 0.)
        assert(next_z > 0.)
        let next = Vector(next_x,next_y,next_z)

        MarchingInfo(i,j,k,0.,dt,next,hit.ray,cell,normal)
    static member GetGridIndex(p:Point,bound:Bound,gridNum:Vector) =
        let dir = p - bound.pMin
        let diag = bound.Diagnal()
        let i = int (floor (dir.x / diag.x * gridNum.x))
        let j = int (floor (dir.y / diag.y * gridNum.y))
        let k = int (floor (dir.z / diag.z * gridNum.z))
        (i,j,k)
    member this.IsInsideGrid(maxi:int,maxj:int,maxk:int) =
        this.i >= 0 && this.i <maxi &&
        this.j >= 0 && this.j <maxj &&
        this.k >= 0 && this.k <maxk
    member this.NextCell() =
        if this.next.x < this.next.y && this.next.x < this.next.z then
            let new_t = this.next.x
            let new_next = Vector(this.next.x+this.d_t.x, this.next.y, this.next.z)
            let normal = float this.sign_x * Vector(-1,0,0)
            MarchingInfo(this.i+this.sign_x,this.j,this.k,new_t,this.d_t,new_next,this.ray,this.cellSize,normal)
        elif this.next.y < this.next.x && this.next.y < this.next.z then
            let new_t = this.next.y
            let new_next = Vector(this.next.x, this.next.y+this.d_t.y, this.next.z)
            let normal = float this.sign_y * Vector(0,-1,0)
            MarchingInfo(this.i,this.j+this.sign_y,this.k,new_t,this.d_t,new_next,this.ray,this.cellSize,normal)
        else
            let new_t = this.next.z
            let new_next = Vector(this.next.x, this.next.y, this.next.z + this.d_t.z)
            let normal = float this.sign_z * Vector(0,0,-1)
            MarchingInfo(this.i,this.j,this.k+this.sign_z,new_t,this.d_t,new_next,this.ray,this.cellSize,normal)

type Grid =
    struct
        val boundingBox : Bound
        val grid : Vector
        val objs : List<IObject3D>
        val info : List<IObject3D>[,,]
        val cellLength : Vector
        val halfCellLength : Vector
        new(box:Bound, nx:int, ny:int, nz:int) =
            let len = box.Diagnal()
            let _cellLen = Vector(len.x/float nx,len.y/float ny,len.z/float nz)
            {
                boundingBox = box;
                grid = Vector(nx,ny,nz);
                objs = List<IObject3D>();
                info = Array3D.init nx ny nz (fun _ _ _ -> new List<IObject3D>());
                cellLength = _cellLen;
                halfCellLength = _cellLen*0.5;
            }
        member private this.GetGridIndex(p:Point) =
            let dir = p - this.boundingBox.pMin
            let diag = this.boundingBox.Diagnal()
            let i = int (floor (dir.x / diag.x * this.grid.x))
            let j = int (floor (dir.y / diag.y * this.grid.y))
            let k = int (floor (dir.z / diag.z * this.grid.z))
            (i,j,k)
        member private this.IsInsideGrid(p:Point) =
            let i,j,k = this.GetGridIndex(p)
            i >= 0 && i <= int this.grid.x &&
            j >= 0 && j <= int this.grid.y &&
            k >= 0 && k <= int this.grid.z
        member private this.InsertHelper_Bound(bound:Bound, obj:IObject3D) =
            let pmin,pmax = bound.pMin,bound.pMax
            let (_min_i,_min_j,_min_k) = this.GetGridIndex(pmin)
            let (_max_i,_max_j,_max_k) = this.GetGridIndex(pmax)
            let min_i = if _min_i = int this.grid.x then _min_i-1 else _min_i
            let min_j = if _min_j = int this.grid.y then _min_j-1 else _min_j
            let min_k = if _min_k = int this.grid.z then _min_k-1 else _min_k
            let max_i = if _max_i = int this.grid.x then _max_i-1 else _max_i
            let max_j = if _max_j = int this.grid.y then _max_j-1 else _max_j
            let max_k = if _max_k = int this.grid.z then _max_k-1 else _max_k
            for i in min_i..max_i do
                for j in min_j..max_j do
                    for k in min_k..max_k do
                        if i >= 0 && i < (int this.grid.x) &&
                           j >= 0 && j < (int this.grid.y) &&
                           k >= 0 && k < (int this.grid.z) then
                            this.info[i,j,k].Add(obj)
        member private this.InsertObject(sphere:Sphere, mat:Matrix4x4, invMat:Matrix4x4) =
            // do with transform
            //let bound = sphere.GetBoundingBox()
            //let newBound = Bound(bound.pMin * mat, bound.pMax * mat)
            //this.InsertHelper_Bound(newBound, sphere)
            let obj :IObject3D =
                if mat = Matrix4x4.MakeIdentity() && invMat = Matrix4x4.MakeIdentity() then
                    sphere
                else
                    Transform(mat,invMat,sphere,sphere.materialIdx)
            let epslon = 0.5 * this.cellLength.Length
            for i in 0..((int this.grid.x) - 1) do
                for j in 0..((int this.grid.y) - 1) do
                    for k in 0..((int this.grid.z) - 1) do
                        let pos = this.boundingBox.pMin +
                                    Vector(double i * this.cellLength.x + this.halfCellLength.x,
                                        double j * this.cellLength.y + this.halfCellLength.y,
                                        double k * this.cellLength.z + this.halfCellLength.z)
                        let distToSphere = (sphere.center-pos).Length
                        if (distToSphere-epslon) <= sphere.radius then
                            this.info[i,j,k].Add(obj)
        member private this.InsertObject(triangle:Triangle, mat:Matrix4x4, invMat:Matrix4x4) =
            //let bound = triangle.GetBoundingBox()
            //let newBound = Bound(bound.pMin * mat,bound.pMax * mat)
            let obj :IObject3D =
                if mat = Matrix4x4.MakeIdentity() && invMat = Matrix4x4.MakeIdentity() then
                    triangle
                else
                    Transform(mat,invMat,triangle,triangle.material)
            let newBound = obj.GetBoundingBox()
            this.InsertHelper_Bound(newBound, obj)
            
        member private this.InsertObject(mesh:TriangleMesh, mat:Matrix4x4, invMat:Matrix4x4) =
            let self = this
            let faces = mesh.faces
            faces |> Array.iter(fun f -> self.InsertObject(f,mat,invMat))
        member private this.InsertObject(transform:Transform, mat:Matrix4x4, invMat:Matrix4x4) =
            this.InsertObject(transform.transformed, transform.mats * mat, invMat * transform.invMats)
        member private this.InsertObject(group:Group, mat:Matrix4x4, invMat:Matrix4x4) =
            let self = this
            let objs = group.objects
            List.iter(fun (obj:IObject3D) -> self.InsertObject(obj,mat,invMat)) objs

        member private this.InsertObject(obj:IObject3D, mat:Matrix4x4, invMat:Matrix4x4) =
            match obj with
            | :? Sphere as sphere -> this.InsertObject(sphere,mat,invMat)
            | :? Plane as plane -> this.objs.Add(plane)
            | :? Triangle as triangle -> this.InsertObject(triangle,mat,invMat)
            | :? TriangleMesh as mesh -> this.InsertObject(mesh,mat,invMat)
            | :? Transform as trans -> this.InsertObject(trans,mat,invMat)
            | :? Group as group -> this.InsertObject(group,mat,invMat)
            | _ -> assert(false)
        member this.HitBound(r:Ray,tmin:float,tmax:float,bound:Bound) =
            let triangles = BoundToTriangle(bound)
            triangles |> Array.map (fun item -> item.Intersect(r, tmin, tmax)) |>
                 Array.minBy(fun hit -> if hit.hit then hit.t else tmax)

        member this.InsertObject(obj:IObject3D) =
            let identity = Matrix4x4.MakeIdentity()
            this.InsertObject(obj,identity,identity)
        member this.Intersect(r:Ray, tmin:float, tmax:float) =
            let aabb = AABB(this.boundingBox.pMin, this.boundingBox.pMax)
            let gridHit =
                if aabb.hit(r,0.,tmax) || Bound.Inside(r.Origin(), this.boundingBox) then
                    let hit = 
                        if Bound.Inside(r.Origin(), this.boundingBox) then
                            Hit(0., r, -1, r.Origin(), Vector())
                        else
                            this.HitBound(r,0.,tmax,this.boundingBox)
                    assert(hit.hit)
                    let mutable matchinfo = MarchingInfo.Init(hit,this.boundingBox,this.grid,this.cellLength)
                    let mutable Break = false
                    let mutable closetHit = Hit()
                    while matchinfo.IsInsideGrid(int this.grid.x,int this.grid.y,int this.grid.z) &&
                            not Break do
                        let i,j,k = (int matchinfo.i), (int matchinfo.j), (int matchinfo.k)
                        let l = this.info[i,j,k]
                        if l.Count > 0 then
                            let tmp = l |> Seq.map(fun o -> o.Intersect(r,tmin,tmax)) |>
                                        Seq.minBy(fun hit -> if hit.hit then hit.t else tmax)
                            if tmp.hit then
                                let t = tmp.t + hit.t
                                closetHit <- tmp
                                Break <- true

                        matchinfo <- matchinfo.NextCell()
                    closetHit
                else
                    Hit()
            let objsHit =
                if this.objs.Count > 0 then
                    this.objs |> Seq.map(fun o -> o.Intersect(r,tmin,tmax)) |>
                        Seq.minBy(fun hit -> if hit.hit then hit.t else tmax)
                else
                    Hit()
            if gridHit.hit && objsHit.hit then
                if gridHit.t < objsHit.t then gridHit else objsHit
            elif gridHit.hit then
                gridHit
            else
                objsHit

        member this.IntersectOnlyGrid(r:Ray, tmin:float, tmax:float) =
            let aabb = AABB(this.boundingBox.pMin, this.boundingBox.pMax)
            let gridHit =
                if aabb.hit(r,tmin,tmax) then
                    let self = this
                    let hit = 
                        if Bound.Inside(r.Origin(), this.boundingBox) then
                        //if this.IsInsideGrid(r.Origin()) then
                            Hit(0., r, -1, r.Origin(), Vector())
                        else
                            this.HitBound(r,tmin,tmax,this.boundingBox)
                    if hit.hit then
                        let mutable matchinfo = MarchingInfo.Init(hit,this.boundingBox,this.grid,this.cellLength)
                        let mutable Break = false
                        let mutable closetHit = Hit()
                        while matchinfo.IsInsideGrid(int this.grid.x,int this.grid.y,int this.grid.z) &&
                                not Break do
                            let i,j,k = (int matchinfo.i), (int matchinfo.j), (int matchinfo.k)
                            let l = self.info[i,j,k]
                            if l.Count > 0 then
                                // draw the grid box to debug
                                let t = matchinfo.t+hit.t
                                closetHit <- Hit(t,r,0,r.PointAtParameter(t),matchinfo.normal)
                                Break <- true
                            matchinfo <- matchinfo.NextCell()
                        closetHit
                    else
                        hit
                else
                    Hit()
            let objsHit =
                if this.objs.Count > 0 then
                    this.objs |> Seq.map(fun o -> o.Intersect(r,tmin,tmax)) |>
                        Seq.minBy(fun hit -> if hit.hit then hit.t else tmax)
                else
                    Hit()
            if gridHit.hit && objsHit.hit then
                if gridHit.t < objsHit.t then gridHit else objsHit
            elif gridHit.hit then
                gridHit
            else
                objsHit
        member this.GetBoundingBox() = this.boundingBox
        interface IObject3D with
            member this.Intersect(r:Ray, tMin:float, tMax:float) = this.Intersect(r,tMin,tMax)
            member this.GetBoundingBox() = this.GetBoundingBox()
    end

type PointLight =
    struct
        val position : Point
        val color : Color
        val attenuation : Vector
        new(pos, c, atten) =
            {
                position = pos;
                color = c;
                attenuation = atten;
            }
        member this.GetIllumination(p:Point) =
            let dir = this.position - p
            let lensqua = dir.LengthSquare
            let len = sqrt(lensqua)
            let attenuation = 1./ (this.attenuation.x +
                                   this.attenuation.y*len +
                                   this.attenuation.z*lensqua)
            (dir/dir.Length,this.color * attenuation)
            //(dir/dir.Length,this.color/len)
            
        interface ILight with
            member this.GetIllumination(p:Point) = this.GetIllumination(p)
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

//[<Struct>]
type Scene =
    val camera : ICamera
    val light : ILight list
    val background : Background
    val materials : MaterialManager
    //val materials : IMaterial list
    val objs : IObject3D list
    val mutable grid : Grid
    new(cam,ls,bg,mats:IMaterial seq,objs) =
        let mat = MaterialManager.GetManager()
        mat.Add(mats)
        {
            camera=cam;light=ls;
            background=bg;materials=mat;
            objs=objs;
            grid = new Grid();
        }
    member this.BuildGrid(nx:int,ny:int,nz:int) =
        let bound = this.objs[0].GetBoundingBox()
        this.grid <- new Grid(bound,nx,ny,nz)
        let self = this
        List.iter (fun (o:IObject3D) -> self.grid.InsertObject(o)) this.objs

    member this.GridIntersect(p:Point2D) =
        let tmin = 0.000001
        let tmax = 9999999.
        let ray = this.camera.GetRay(p)
        let hit = this.grid.Intersect(ray, tmin, tmax)
        if hit.hit then
            let matIdx = hit.material
            let mutable finalColor = Color()
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)
                let objColor = this.materials[matIdx].Color(hit, toLight)
                //let d = toLight.Dot(hit.normal)
                finalColor <- finalColor + c * objColor
            finalColor, hit.t
            //Color(abs hit.normal.x, abs hit.normal.y, abs hit.normal.z), hit.t
        else
            Color(), 0.

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
            let material = this.materials[matIdx]
            let mutable finalColor = Color()
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)
                let objColor = material.Color(hit,toLight)
                //let d = toLight.Dot(hit.normal)
                finalColor <- finalColor + c * objColor
            this.GetAmbientLight() * material.NoLightColor(hit) + finalColor, hit.t
        else
            this.background.bgColor, 0.
    member this.GetHit(ray:Ray, tmin:float, tmax:float) =
        this.objs |> List.map (fun item -> item.Intersect(ray, tmin, tmax)) |>
                    List.minBy(fun hit -> if hit.hit then hit.t else tmax)
    member this.GetColorGrid(ray:Ray, tmin:float, tmax:float) =
        let hit = this.grid.IntersectOnlyGrid(ray, tmin, tmax)
        if hit.hit then
            let matIdx = hit.material
            let material = this.materials[matIdx]
            let mutable finalColor = this.background.bgColor * material.NoLightColor(hit)
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)
                let objColor = material.Color(hit, toLight)
                finalColor <- finalColor + c * objColor
            hit, finalColor
            //hit, Color(abs hit.normal.x, abs hit.normal.y, abs hit.normal.z)
        else
            hit, this.background.bgColor
    member this.IntersectFast(ray:Ray, tmin:float, tmax:float) =
        this.grid.Intersect(ray,tmin,tmax)
    member this.GetColorFast(ray:Ray, tmin:float, tmax:float) =
        let hit = this.grid.Intersect(ray, tmin, tmax)
        if hit.hit then
            let matIdx = hit.material
            let material = this.materials[matIdx]
            let objColor = material.NoLightColor(hit)
            let mutable finalColor = Color()
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)

                // Check is the hit point in shadow
                let shadowRay = Ray(hit.point, toLight)
                //*tmax must lest than light distance*/
                let shadowhit = this.grid.Intersect(shadowRay, tmin, tmax)
                if not shadowhit.hit then
                    let d = max 0.0 (toLight.Dot(hit.normal))
                    finalColor <- finalColor + d * c * objColor
                    //let d = max 0.0 (toLight.Dot(hit.normal))
                    ////let q = max 0.0 (toLight.Dot(-hit.ray.Direction()))
                    //let reflectDir = material.MirrorDirection(hit.normal, ray.Direction())
                    //let _, reflectColor = this.GetColorFast(Ray(hit.point,reflectDir), tmin, tmax)
                    //let q = max 0.0 (reflectDir.Dot(toLight))
                    //let specu = if material.isSpecular then material.specularColor * Math.Pow(q, material.exponent) else Color()
                    //let reflec = 0.7 * (max (reflectDir.Dot(hit.normal)) 0.) * material.ReflectiveColor() * reflectColor
                    //finalColor <- finalColor + specu + reflec + d * c * objColor
            finalColor <- finalColor + this.GetAmbientLight() * objColor
            //hit, finalColor
            hit, Color(min 1. finalColor.r, min 1. finalColor.g, min 1. finalColor.b)
        else
            hit, this.background.bgColor
    member this.GetColor(ray:Ray, tmin:float, tmax:float) =
        let hit = this.GetHit(ray, tmin, tmax)
        if hit.hit then
            let matIdx = hit.material
            let material = this.materials[matIdx]
            let mutable finalColor = Color()
            for light:ILight in this.GetLight() do
                let toLight, c = light.GetIllumination(hit.point)

                // Check is the hit point in shadow
                let shadowRay = Ray(hit.point, toLight)
                //*tmax must lest than light distance*/
                let shadowhit = this.GetHit(shadowRay, tmin, tmax)
                if not shadowhit.hit then
                    finalColor <- finalColor + material.Color(hit, toLight)
                    
                //else
                //    let shadowMaterial = this.materials[shadowhit.material]
                //    if shadowMaterial.IsTransmitted() then
                //        let _, shadowColor = this.GetColor(shadowRay, tmin, tmax)
                //        finalColor <- finalColor + shadowColor * shadowMaterial.transmittedColor
            finalColor <- finalColor + this.GetAmbientLight() * material.NoLightColor(hit)
            //hit, finalColor
            hit, Color(min 1. finalColor.r, min 1. finalColor.g, min 1. finalColor.b)
        else
            hit, this.background.bgColor
    member this.GetLight() = this.light
    member this.GetAmbientLight() = this.background.ambientColor

type RayTracer =
    val scene: Scene
    val maxBounces : int
    val cutoffWeight : float
    val isDrawFast : bool
    val isDrawGrid : bool
    new(s, bounces, cutoff, fast, showGrid) =
        {
            scene=s;
            maxBounces=bounces;
            cutoffWeight=cutoff;
            isDrawFast=fast;
            isDrawGrid=showGrid;
        }
    member this.Trace(p:Point2D) =
        let ray = this.scene.camera.GetRay(p)
        if this.isDrawGrid then
            let hit, col = this.scene.GetColorGrid(ray, 1e-6, 9999999.0)
            //Color(hit.normal.x, hit.normal.y, hit.normal.z), hit.t
            Color(min 1. col.r, min 1. col.g, min 1. col.b), hit.t
        elif this.isDrawFast then
            //let hit, col = this.TraceRayFast(ray, 1e-6, 9999999.0, 0, 1., 0.)
            let hit, col = this.TraceRaySuperFast(ray, 1e-6, 9999999.0, 0, 1., 0.)
            Color(min 1. col.r, min 1. col.g, min 1. col.b), hit.t
        else
            let hit, col = this.TraceRay(ray, 1e-6, 9999999.0, 0, 1., 0.)
            let mutable finalCol = col

            Color(min 1. finalCol.r, min 1. finalCol.g, min 1. finalCol.b), hit.t
    member this.TraceRay(r:Ray, tmin:float, tmax:float,
                         bounces:int, weight:float, indexOfRefraction:float) : Hit * Color =
        if bounces < this.maxBounces then
            let hit, col = this.scene.GetColor(r, tmin, tmax)
            let mutable finalColor = col
            if hit.hit then
                let material = this.scene.materials[hit.material]
                let isReflect, reflectDir, reflectColor = material.Reflective(hit)
                if isReflect then
                    let refRay = Ray(hit.point, reflectDir)
                    let _, secondCol = this.TraceRay(refRay, tmin, tmax, bounces+1, weight*0.4, indexOfRefraction)
                    finalColor <- finalColor + weight * reflectColor * secondCol

                let isTransmit, transDir, transColor = material.Transmitive(hit)
                if isTransmit then
                    let transRay = Ray(hit.point, transDir)
                    let _, secondCol = this.TraceRay(transRay, tmin, tmax, bounces+1, weight*0.4, indexOfRefraction)
                    finalColor <- finalColor + transColor * secondCol
            hit, finalColor
        else
            Hit(), Color()
    member this.TraceRaySuperFast(r:Ray, tmin:float, tmax:float,
                            bounces:int, weight:float, indexOfRefraction:float) : Hit * Color =
        if bounces < this.maxBounces then
            let hit = this.scene.IntersectFast(r,tmin,tmax)
            let mutable finalColor = Color()
            if hit.hit then
                let material = this.scene.materials[hit.material]
                for light:ILight in this.scene.GetLight() do
                    let toLight, c = light.GetIllumination(hit.point)
                    let shadowRay = Ray(hit.point, toLight)
                    let hitLight = this.scene.IntersectFast(shadowRay,1e-6,99999.)
                    if not hitLight.hit then
                        finalColor <- finalColor + c * material.Color(hit, toLight)
 
                let isReflect, reflectDir, reflectColor = material.Reflective(hit)
                if isReflect then
                    let reflectRay = Ray(hit.point, reflectDir)
                    let reflectHit, col = this.TraceRaySuperFast(reflectRay,tmin,tmax,bounces+1,weight*0.9,0.)
                    //if reflectHit.hit then
                    finalColor <- finalColor + col * reflectColor
                    
                let isTransmit, refractDir, refractColor = material.Transmitive(hit)
                if isTransmit then
                    let refractRay = Ray(hit.point, refractDir)
                    let _, col = this.TraceRaySuperFast(refractRay,tmin,tmax,bounces+1,weight*0.9,0.)
                    finalColor <- finalColor + col * refractColor

            hit, finalColor
        else
            Hit(), Color()
    member this.TraceRayFast(r:Ray, tmin:float, tmax:float,
                         bounces:int, weight:float, indexOfRefraction:float) : Hit * Color =
        if bounces < this.maxBounces then
            let hit, col = this.scene.GetColorFast(r, tmin, tmax)
            let mutable finalColor = col
            if hit.hit then
                let material = this.scene.materials[hit.material]
                let isReflect, reflectDir, reflectColor = material.Reflective(hit)
                if isReflect then
                    let refRay = Ray(hit.point, reflectDir)
                    let refHit, secondCol = this.TraceRayFast(refRay, tmin, tmax, bounces+1, weight*0.4, indexOfRefraction)
                    //if refHit.hit then
                    finalColor <- finalColor + weight *  reflectColor * secondCol
                let isTransmit, refractDir, refractColor = material.Transmitive(hit)
                if isTransmit then
                    let transRay = Ray(hit.point, refractDir)
                    let _, secondCol = this.TraceRayFast(transRay, tmin, tmax, bounces+1, weight*0.4, indexOfRefraction)
                    finalColor <- finalColor + refractColor * secondCol
            hit, finalColor
        else
            Hit(), Color()

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
let toPointLight((p,c),a) =
    PointLight(p,c,a)
let pPointLight =
    keyword_PointLight >>.
    between beginScope endScope
            (keyword_Position .>> spaces >>.pPoint .>> spaces .>>
             keyword_Color .>> spaces .>>. pColor .>> spaces .>>
             keyword_Attenuation .>> spaces .>>. pVector .>> spaces
             |>> toPointLight)

let pTransparentColor =
    keyword_TransparentColor .>> spaces >>. pColor .>> spaces
let pIndexOfRefraction =
    keyword_IndexOfRefraction .>> spaces >>. pfloat .>> spaces
let pDiffuseColor =
    keyword_DiffuseColor .>>spaces >>. pColor .>> spaces
let pReflectiveColor =
    keyword_ReflectiveColor .>> spaces >>. pColor .>> spaces
let pSpecularColor =
    keyword_SpecularColor .>> spaces >>. pColor .>> spaces
let pExponent =
    keyword_Exponent .>> spaces >>. pfloat .>> spaces
let MaterialNoSpecuFlatten (((c,t),r),i) =
    c,t,r,i
let MaterialNoRefractionFlatten ((d,s),e) =
    d,s,e
let pMaterialNoSpecu =
    pDiffuseColor .>>.
    pTransparentColor .>>.
    pReflectiveColor .>>.
    pIndexOfRefraction |>> MaterialNoSpecuFlatten
let pMaterialNoRefraction =
    pDiffuseColor .>>.
    pSpecularColor .>>.
    pExponent |>> MaterialNoRefractionFlatten
let MaterialFlatten(((((diffu,spec), exp), trans), reflec), idx) =
    diffu, spec, exp, trans, reflec, idx
let pMaterialWithRefTrans =
    pDiffuseColor .>>.
    pSpecularColor .>>.
    pExponent .>>.
    pTransparentColor .>>.
    pReflectiveColor .>>.
    pIndexOfRefraction |>> MaterialFlatten
let pAssetMaterial =
    keyword_Material >>.
    between beginScope endScope
            ((attempt (pMaterialWithRefTrans |>> AssetMaterial) <|>
             attempt (pMaterialNoRefraction |>> AssetMaterial) <|>
             attempt (pMaterialNoSpecu |>> AssetMaterial) <|>
             attempt (pDiffuseColor .>>. pReflectiveColor |>> AssetMaterial) <|>
             attempt (pDiffuseColor |>> AssetMaterial)) |>> (fun x -> x:>IMaterial))

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
        if l.Length > 0 then
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
            match obj with
            | :? Transform as trans ->
                Transform(trans.mats * mats, invMats * trans.invMats, trans.transformed, trans.material)
            | _ ->
                Transform(mats,invMats,obj,material)
        else
            let mat = Matrix4x4.MakeIdentity()
            let obj = toObject(ast, material)
            Transform(mat,mat,obj,material)
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

let pMaterialIndex =
    keyword_materialIndex >>. spaces >>. pint32 .>> spaces

let pCheckerboardTransform : Parser<Transform, unit> =
    let p : Parser<ObjectAST, unit>=
        fun stream ->
            Reply(ObjectAST.Sphere_node(Point(),0.))
    let tmp : Parser<ObjectAST, unit>=
        keyword_Transform >>.
        between beginScope endScope
                (pTransformation .>> spaces .>>. p) |>> ObjectAST.Transform_node
    fun stream ->
        let reply = tmp stream
        if reply.Status = Ok then
            let trans = toObject(reply.Result, 0) :?> Transform
            Reply(trans)
        else // reconstruct error reply
            Reply(reply.Status, reply.Error)
        

let CheckerboardFlatten((trans,id1),id2) = trans,id1,id2

let pCheckerboard =
    keyword_Checkerboard >>.
    between beginScope endScope
        ((pCheckerboardTransform .>>. pMaterialIndex .>>. pMaterialIndex) |>>
            CheckerboardFlatten |>> CheckerBoard |>> (fun x -> x :> IMaterial))

let pOctaves =
    keyword_Octaves >>. spaces >>. pint32 .>> spaces
let pNoise =
    keyword_Noise >>.
    between beginScope endScope
        (pCheckerboardTransform .>>. pMaterialIndex .>>. pMaterialIndex .>>. pOctaves |>>
            (fun (((t,i1),i2),oc) -> t,i1,i2,oc) |>> Noise |>> (fun x -> x :> IMaterial))

let pFrequency =
    keyword_Frequency >>. spaces >>. pfloat .>> spaces
let pAmplitude =
    keyword_Amplitude >>. spaces >>. pfloat .>> spaces
let pMarble =
    keyword_Marble >>.
    between beginScope endScope
        (pCheckerboardTransform .>>. pMaterialIndex .>>. pMaterialIndex .>>. pOctaves .>>.
            pFrequency .>>. pAmplitude |>>
            (fun (((((t,i1),i2),oct),freq),ampl) -> t,i1,i2,oct,freq,ampl) |>>
            Marble |>> (fun x -> x :> IMaterial))
let pMaterial =
    choice [
        pAssetMaterial
        pCheckerboard
        pNoise
        pMarble
    ]

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
            (pNumLights .>> spaces >>.
             many ((pDirectionalLight |>> toILight) <|>
                   (pPointLight |>> toILight)))
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
    val drawFast : bool
    val drawGrid : bool
    val minDepth : float
    val maxDepth : float
    val depthField : float
    val bounces : int
    val weight : float
    val nx : int
    val ny : int
    val nz : int
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
        let bouncesIdx = CommandIndex(ls, "-bounces")
        let _bounces =if bouncesIdx <> -1 then int ls[bouncesIdx+1] else 20
        let weightIdx = CommandIndex(ls, "-weight")
        let _weight = if weightIdx <> -1 then float ls[weightIdx+1] else 0.1
        let gridIdx = CommandIndex(ls, "-grid")
        let _hasGrid = gridIdx <> -1
        let visualizeGridIdx = CommandIndex(ls, "-visualize_grid")
        let isDrawGrid = visualizeGridIdx <> -1
        {
            inputFile = ls[inputIdx+1];
            screenSize = int ls[sizeIdx+1];
            drawNormal = normalIdx <> -1;
            drawDepth = bHasDepth;
            drawFast = _hasGrid && not isDrawGrid;
            drawGrid = isDrawGrid;
            minDepth = _minDepth;
            maxDepth = _maxDepth;
            depthField = _maxDepth - _minDepth;
            bounces = _bounces;
            weight = _weight;
            nx = if _hasGrid then int ls[gridIdx+1] else 0;
            ny = if _hasGrid then int ls[gridIdx+2] else 0;
            nz = if _hasGrid then int ls[gridIdx+3] else 0;
        }


let ParseScene (parseinfo:RayTracer_Info) =
    let filename = parseinfo.inputFile
    let lines = System.IO.File.ReadAllText(filename)
    match run pScene lines with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg
        assert(false)
        Scene(OrthographicCamera(Point(),Vector(),Vector(),0),[],Background(), [], [])

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
    
let LoadScene_Assignment6() =
    // Source file(eg. scene1_01.txt) from https://groups.csail.mit.edu/graphics/classes/6.837/F04/assignments/assignment6/
    
    //let info = RayTracer_Info("raytracer -input scene6_01_sphere.txt -output output6_01b.tga -size 200 200 -grid 10 10 10 -stats")
    //let info = RayTracer_Info("raytracer -input scene6_01_sphere.txt -output output6_01c.tga -size 200 200 -grid 10 10 10 -visualize_grid")
    //let info = RayTracer_Info("raytracer -input scene6_02_sphere_triangles.txt -output output6_02c.tga -size 200 200 -stats -shadows")
    //let info = RayTracer_Info("raytracer -input scene6_02_sphere_triangles.txt -output output6_02b.tga -size 200 200 -grid 10 10 10 -stats")
    //let info = RayTracer_Info("raytracer -input scene6_02_sphere_triangles.txt -output output6_02e.tga -size 200 200 -grid 10 10 10 -visualize_grid")
    //let info = RayTracer_Info("raytracer -input scene6_03_sphere_plane.txt -output output6_03d.tga -size 200 200 -grid 10 10 10 -depth 0 7 -stats -shadows")
    //let info = RayTracer_Info("raytracer -input scene6_04_bunny_mesh_200.txt -output output6_04b.tga -size 200 200 -grid 10 10 7 -stats")
    //let info = RayTracer_Info("")
    //let info = RayTracer_Info("")
    //let info = RayTracer_Info("raytracer -input scene6_07_bunny_mesh_40k.txt -output output6_07.tga -size 200 200 -grid 40 40 33 -stats -shadows")
    //let info = RayTracer_Info("raytracer -input scene6_08_scale_translate.txt -size 200 200 -output output6_08b.tga -grid 15 15 15")
    //let info = RayTracer_Info("raytracer -input scene6_08_scale_translate.txt -size 200 200 -output output6_08c.tga -grid 15 15 15 -visualize_grid")
    //let info = RayTracer_Info("raytracer -input scene6_09_rotated_triangles.txt -size 200 200 -output output6_09b.tga -grid 15 15 9")
    //let info = RayTracer_Info("raytracer -input scene6_09_rotated_triangles.txt -size 200 200 -output output6_09c.tga -grid 15 15 9 -visualize_grid")
    //let info = RayTracer_Info("raytracer -input scene6_10_nested_transformations.txt -size 200 200 -output output6_10b.tga -grid 30 30 30")
    //let info = RayTracer_Info("raytracer -input scene6_11_mirrored_floor.txt -size 200 200 -output output6_11b.tga -shadows -bounces 10 -weight 0.01 -grid 40 10 40 -stats")
    //let info = RayTracer_Info("raytracer -input scene6_12_faceted_gem.txt -size 200 200 -output output6_12b.tga -shadows -shade_back -bounces 5 -weight 0.01 -grid 20 20 20 -stats")
    //let info = RayTracer_Info("raytracer -input scene6_13_checkerboard.txt -size 200 200 -output output6_13.tga -shadows ")
    //let info = RayTracer_Info("raytracer -input scene6_14_glass_sphere.txt -size 200 200 -output output6_14.tga -shadows -shade_back -bounces 5 -weight 0.01 -grid 20 20 20")
    //let info = RayTracer_Info("raytracer -input scene6_15_marble_cubes.txt -size 300 300 -output output6_15.tga ")
    //let info = RayTracer_Info("")
    //let info = RayTracer_Info("raytracer -input scene6_17_marble_vase.txt -size 300 300 -output output6_17a.tga -grid 15 30 15 -bounces 2 -shadows")
    //let info = RayTracer_Info("raytracer -input scene6_17_marble_vase.txt -size 300 300 -output output6_17b.tga -grid 15 30 15 -visualize_grid")
    let info = RayTracer_Info("raytracer -input scene6_18_6.837_logo.txt -size 400 200 -output output6_18a.tga -shadows -shade_back -bounces 5 -weight 0.01 -grid 80 30 3")
    //let info = RayTracer_Info("raytracer -input scene6_18_6.837_logo.txt -size 400 200 -output output6_18b.tga -grid 80 30 3 -visualize_grid")
    
    let screen_size = info.screenSize
    let scene = ParseScene info
    if info.drawFast || info.drawGrid then
        scene.BuildGrid(info.nx,info.ny,info.nz)
    let screen = Screen(screen_size, screen_size)
    let normal = Screen(screen_size, screen_size)
    let depth = Screen(screen_size, screen_size)

    let tracer = RayTracer(scene, info.bounces, info.weight, info.drawFast, info.drawGrid)
    // render begin
    [|
        for x in 0..screen_size-1 do
            for y in 0..screen_size-1 do
                yield x,y
    |] |>
    //Array.iter (fun (x,y) ->
    Array.Parallel.iter (fun (x,y) ->
        let p = Point2D(float x / float screen_size, float y / float screen_size)
        let c,d = tracer.Trace(p)
        //let c,d = scene.GetColorDepth(p)
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
    )

    ShowMat(screen, "Colored image")
    if info.drawNormal then ShowMat(normal, "Normal image")
    if info.drawDepth then ShowMat(depth, "Depth image")
    Cv2.WaitKey() |> ignore
