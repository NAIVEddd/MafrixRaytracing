module RayTracing3

open System.IO
open OpenCvSharp
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
open Engine.Core.Shapes.Trangle
open Engine.Core.Shapes.Rect
open Engine.Core.Shapes.Box
open Engine.Core.Shapes.CircleAreaLightObject
open Engine.Core.Samplers.JitteredSampler
open Engine.Core.Materials.Lambertian
open Engine.Core.Materials.GlossySpecular
open Engine.Core.Materials.PerfectSpecular

open Engine.Core.Accels.BvhNode
open Engine.Core.World

open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Interfaces.ICamera
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.ISampler
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld
open Engine.Core.Tracers.RayCast
open Engine.Core.Tracers.Whitted
open Engine.Core.Tracers.PathTracer
open Engine.Core.Lights.Ambient
open Engine.Core.Lights.PointLight
open Engine.Core.Lights.Directional
open Engine.Core.Lights.AreaLight

open System

let ListHit(items:IHitable[], r:Ray, tmin:float, tmax:float) =
    items |> Array.map(fun i -> i.Hit(r,tmin,tmax)) |> Array.minBy(fun hitrecord -> (
        if hitrecord.bHit then hitrecord.t else tmax))


let Reflect(v:Vector, n:Vector) = v - 2.0*v.Dot(n)*n
let Refract(v:Vector, n:Vector, ni_over_nt:float) =
    let uv = v.Normalize
    let dt = uv.Dot(n)
    let discriminant = 1.0 - ni_over_nt*ni_over_nt*(1.0-dt*dt)
    if discriminant > 0 then
        (true, ni_over_nt*(v-n*dt) - n*sqrt(discriminant))
    else
        (false, Reflect(v, n))
            
type Dielectric(ri:float) =
    let ref_idx = ri
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        let reflected = Reflect(ray.Direction(), hit.normal)
        let mutable outward_normal = Vector()
        let mutable ni_over_nt = 0.0
        if ray.Direction().Dot(hit.normal) > 0 then
            outward_normal <- -hit.normal
            ni_over_nt <- ref_idx
        else
            outward_normal <- hit.normal
            ni_over_nt <- 1.0 / ref_idx
        let (isrefract, ref_dir) = Refract(ray.Direction(), outward_normal, ni_over_nt)
        if isrefract then
            Color(0.25,0.25,0.25), Ray(hit.point, ref_dir)
        else
            Color(0.25,0.25,0.25), Ray(hit.point, reflected)
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = indirect * Color(0.5,0.5,0.5) / 0.25
        member this.BaseColor() = Color(0.95,0.95,0.95)
        member this.Emit() = Color()

// n1 and n2 is index of refraction
type FresnelDielectrics(n1:float,n2:float) =
    let n1 = n1
    let n2 = n2
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) =
            let reflected = Reflect(ray.Direction(), hit.normal)
            let (isrefract, ref_dir) = Refract(ray.Direction(), hit.normal, n1 / n2)
            let cos_refl = reflected.Dot(hit.normal)
            let cos_refr = ref_dir.Dot(hit.normal)

            let para = (n2 * cos_refl - n1 * cos_refr) / (n2 * cos_refl + n1 * cos_refr)
            let perp = (n1 * cos_refl - n2 * cos_refr) / (n1 * cos_refl + n2 * cos_refr)
            let percent_reflect = 0.5 * (para*para + perp*perp)
            let percent_transmi = 1. - percent_reflect
            if percent_reflect > percent_transmi then
                Color(1,1,1), Ray(hit.point, reflected)
            else
                Color(1,1,1), Ray(hit.point, ref_dir)
        member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = indirect
        member this.BaseColor() = Color()
        member this.Emit() = Color()

type Matte(ka:float, kd:float, color:Color) =
    let ambient_brdf = new Lambertian1(ka, color)
    let diffuse_brdf = new Lambertian1(kd, color)
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            false,Vector(),Ray()
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.PathShade(hit, w, depth) =
            let world = w:?>IWorld
            let mutable wi = Vector()
            let mutable pdf = 0.0
            let f = diffuse_brdf.Sample_f(hit, &wi, -hit.hitRay.Direction(),0,0,&pdf)
            let ndotwi = hit.normal.Dot(wi)
            let reflected = Ray(hit.p, wi)
            let t = world.GetTracer().TraceRay(reflected, depth+1)
            f * t * ndotwi / pdf
        member this.Shade(hit, w:obj) =
            let world = w:?>IWorld
            let wo = - hit.hitRay.Direction()
            let mutable l = ambient_brdf.rho(wo, 1,1) * world.GetAmbientLight().L(hit)
            for light in (world.GetLights()) do
                let wi = light.GetDirection(hit)
                let ndotwi = hit.normal.Dot(wi)
                if ndotwi > 0.0 then
                    let in_shadow =
                        if light.CastsShadows() then
                            let shadowray = Ray(hit.p,wi)
                            light.InShadow(hit, shadowray, world)
                        else
                            false
                    if not in_shadow then
                        l <- l + diffuse_brdf.f(hit, wi, wo) * light.L(hit) * ndotwi
            l

type Phong(ka:float, kd:float, ks:float, spec:float, color:Color) =
    let ambient_brdf = new Lambertian1(ka, color)
    let diffuse_brdf = new Lambertian1(kd, color)
    let specular_brdf = new GlossySpecular(ks, spec, color)
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            false,Vector(),Ray()
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.PathShade(hit, w, depth) = Color()
        member this.Shade(hit, w:obj) =
            let world = w:?>IWorld
            let wo = - hit.hitRay.Direction()
            let mutable l = ambient_brdf.rho(wo, 1,1) * world.GetAmbientLight().L(hit)
            for light in (world.GetLights()) do
                let wi = light.GetDirection(hit)
                let ndotwi = hit.normal.Dot(wi)
                if ndotwi > 0.0 then
                    let in_shadow =
                        if light.CastsShadows() then
                            let shadowray = Ray(hit.p,wi)
                            light.InShadow(hit, shadowray, world)
                        else
                            false
                    if not in_shadow then
                        l <- l + (diffuse_brdf.f(hit, wi, wo) + specular_brdf.f(hit, wi, wo)) * light.L(hit) * ndotwi
            l

type Emissive(ls:float, normal:Vector, color:Color) =
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        let dir = GetRandomInHemisphere(normal)
        color, Ray(hit.point, dir.Normalize)
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = Color()
        member this.BaseColor() = color
        member this.Emit() = color

type Reflective(a:Vector, ls:float, color:Color, tracer:ITracer) =
    let albedo = a
    let phong :IMaterial = Phong(0.2,0.65,0.1,10,color)
    let reflective_brdf = PerfectSpecular(ls,color)
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let mutable wi = Vector()
            let mutable pdf = 0.0
            let col = reflective_brdf.Sample_f(hit, &wi, -ray.Direction(), 0,0,&pdf)
            let reflected = Reflect(-ray.Direction(), hit.normal)
            let scattered = Ray(hit.p, wi)// + GetRandomInUnitSphere())
            scattered.Direction().Dot(hit.normal) > 0, Vector(col.r,col.g,col.b), scattered
            
            //let scattered = Ray(hit.p, reflected)// + GetRandomInUnitSphere())
            //scattered.Direction().Dot(hit.normal) > 0, albedo, scattered
        member this.PathShade(hit,w, depth) =
            let wo = -hit.hitRay.Direction()
            let mutable wi = Vector()
            let mutable pdf = 0.0
            let fr = reflective_brdf.Sample_f(hit,&wi,wo,0,0,&pdf)
            let reflected = Ray(hit.p, wi)
            let t = (w:?>IWorld).GetTracer().TraceRay(reflected, depth+1)
            fr * t * (hit.normal.Dot(wi)) / pdf
        member this.Shade(hit,w,depth,wo) =
            wo <- Reflect(-hit.hitRay.Direction(), hit.normal)
            let mutable wi = Vector()
            let mutable pdf = 0.0
            let col = reflective_brdf.Sample_f(hit, &wi, -hit.hitRay.Direction(), 0,0,&pdf)
            let scattered = Ray(hit.p, wi)// + GetRandomInUnitSphere())
            0.2 * phong.Shade(hit,w) + 0.8 * col * hit.normal.Dot(wi) * tracer.TraceRay(scattered, depth+1)
        member this.Shade(hit, w:obj) =
            tracer.TraceRay(hit.hitRay, 0)

//let Light = NewPointLight(Point(0,1.98,0), Color(0.78,0.78,0.78)):>INewLight
let AreaLight = NewAreaLight(Point(-0.24,1.98,0.16),Point(-0.24,1.98,-0.22),Point(0.23,1.98,-0.22),Point(0.23,1.98,0.16),Vector(0,-1,0),Color(20,20,20)):>INewLight

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

let DrawScreenByPixel(cam:Camera, bvh:Bvh, objs:INewHitable[], screen:Screen, arr:(int*int) array, samplePerPixel:int) =
    let ns = samplePerPixel
    let nx = cam.width
    let ny = cam.height
    let trace = NewPathTracer(bvh,3,AreaLight) :> INewTracer
    //arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
    arr |> Array.Parallel.iter(fun (i,j) ->
    //arr |> Array.iter(fun (i,j) ->
            let mutable col = Color()
            for s = 0 to ns-1 do
                let u = (float i) / float nx
                let v = (float j) / float ny
                let u = float i
                let v = float j
                let ray = cam.GetRay(u, v)
                let c = trace.TraceRay(ray)
                col <- col + c
            col <- col / float ns
            col <- ACESFilmToneMapping(col)
            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
            let ir = int (255.99*col.r)
            let ig = int (255.99*col.g)
            let ib = int (255.99*col.b)
            screen[i,(ny-1)-j] <- Color(ir,ig,ib), 1)

let DoRayTrace3() =
    let result = LoadObjModel("CornellBox-Original.obj")
    let nx = 300
    let ny = 300
    let ns = 20

    let cam = new Camera(Point(0, 1, 3),Vector(0,0,-1), 120.0, 0.5, 1000.0, nx, ny)

    let screen = new Screen(nx, ny)
    let mat = new Mat(Size(nx, ny), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    let (w,h) = screen.Size()
    let arr = Array.allPairs [|0..w-1|] [|0..h-1|]

    //MaterialManager.GetManager()[5] <- SpecularTransmission(Color(0.6,0.6,0.6), 1.0, 1.51)

    let objs = result.faces.ToArray()
    let bvh = Bvh.Build(objs)
    let RenderWithSingleFrame() =
        //random shuffer pixels
        let rand = new System.Random()
        let swap x y (a: 'a []) =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp
        arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
        //DrawScreenByChunk(cam, tracer, screen, arr, ChunkPerThread, ns)
        DrawScreenByPixel(cam, bvh, objs, screen, arr, ns)
        arr |> Array.iter(fun(x,y) ->
            let c = screen.Pixel(x,y)
            let vec = Vec3b(byte c.b, byte c.g, byte c.r)
            indexer[y,x] <- vec)
            
        Cv2.ImShow("Ray tracing", mat)
        Cv2.WaitKey() |> ignore

    //RenderWithContinus()
    RenderWithSingleFrame()