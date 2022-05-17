module RayTracing3

open System.IO
open OpenCvSharp
open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Core.Ray
open Engine.Model.Obj
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

let GetRandomInUnitSphere(nm:Vector) =
    let mutable p = Vector(20,20,20)
    let rand = new System.Random()
    while p.Dot(p) >= 1.0 || nm.Dot(p) <= 0. do
        p <- 2.0 * Vector(rand.NextDouble(), rand.NextDouble(), rand.NextDouble()) - Vector(1,1,1)
    p

let Reflect(v:Vector, n:Vector) = v - 2.0*v.Dot(n)*n
let Refract(v:Vector, n:Vector, ni_over_nt:float) =
    let uv = v.Normalize
    let dt = uv.Dot(n)
    let discriminant = 1.0 - ni_over_nt*ni_over_nt*(1.0-dt*dt)
    if discriminant > 0 then
        (true, ni_over_nt*(v-n*dt) - n*sqrt(discriminant))
    else
        (false, Reflect(v, n))


let INVPI = 1. / Math.PI
type Lambertian(a:Color) =
    let albedo = a
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        assert(abs(1.-hit.normal.Length) < 1e-6)
        let target = (GetRandomInUnitSphere(hit.normal)).Normalize
        //let target = (hit.normal + GetRandomInUnitSphere(hit.normal)).Normalize
        let scattered = Ray(hit.point, target)
        albedo * INVPI, scattered
    member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) =
        let ei = hit.normal.Dot(r.Direction())
        assert(ei >= 0.)
        indirect * 2. * Math.PI * ei
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit,r,indirect) = this.Shade(hit,r,indirect)
        member this.BaseColor() = a
        member this.Emit() = Color()
type Metal(a:Color, f:float) =
    let albedo = a
    let fuzz = if f < 1.0 then f else 1.0
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        let reflected = Reflect(ray.Direction(), hit.normal)
        let dir = (reflected + fuzz*GetRandomInUnitSphere(hit.normal)).Normalize
        let scattered = Ray(hit.point, dir)
        albedo, scattered
    member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = indirect
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit,r,indirect) = this.Shade(hit,r,indirect)
        member this.BaseColor() = a
        member this.Emit() = Color()
            
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

let GetCornellBox() : IHitable[] =
    let red = Matte(1., 1., Color(0.65, 0.05, 0.05))
    let white = Matte(1., 1., Color(0.73, 0.73, 0.73))
    let green = Matte(1., 1., Color(0.12, 0.45, 0.15))
    let light = Emissive(15., Vector(0, -1, 0), Color(1,1,1))
    let r1 = flip_normals(yz_rect(0, 555, 0, 555, 555, green))
    let r2 = yz_rect(0, 555, 0, 555, 0, red)
    //let r3 = flip_normals(xz_rect(213, 343, 227, 323, 554, light))
    let r4 = xz_rect(0, 555, 0, 555, 0, white)
    let r5 = flip_normals(xy_rect(0, 555, 0, 555, 555, white))
    let r6 = flip_normals(xz_rect(0, 555, 0, 555, 555, white))
    let b1 = Translate(Rotate_y(Box(Vector(0, 0, 0), Vector(165, 165, 165), white), -18), Vector(130, 0, 65))
    let b2 = Translate(Rotate_y(Box(Vector(0, 0, 0), Vector(165, 330, 165), white), 15), Vector(265, 0, 295))
    [||]
    //[|r1;r2;r3;r4;r5;r6;b1;b2|]

let GetSpheres() : INewHitable[] =
    let m1 = Lambertian(Color(0.725,0.71,0.68))
    let mats : INewMaterial[] = [|m1;|]
    MaterialManager.GetManager().Add(mats)

    let s1 = NewSphere(Point(0,-100,0), 100, 0)
    Array.append [|s1;|]
        [|
            for i in 0..20 do
                let r = System.Random.Shared.NextDouble() * 0.5
                let x = (System.Random.Shared.NextDouble()-0.5) * 2.
                let z = (System.Random.Shared.NextDouble()-0.5) * 2.
                yield (NewSphere(Point(x,r,z),r,0):>INewHitable)
        |]


let GetObjects() : INewHitable[] =
    let m1 = Lambertian(Color(0.725,0.71,0.68))
    let m2 = Lambertian(Color(0.725,0.71,0.68))
    let m3 = Lambertian(Color(0.725,0.71,0.68))
    let m4 = Lambertian(Color(0.14,0.45,0.091))
    let m5 = Lambertian(Color(0.63,0.065,0.05))
    let m6 = Emissive(1., Vector(0,-1,0), Color(1,1,1))
    let m7 = Lambertian(Color(0.725,0.71,0.68))
    let m8 = Lambertian(Color(0.725,0.71,0.68))
    let m9 = Metal(Color(0.5,0.5,0.5), 0)
    let m10 = Dielectric(1.61)
    let mats : INewMaterial[] = [|m1;m2;m3;m4;m5;m6;m7;m8;m9;m10;|]
    MaterialManager.GetManager().Add(mats)

    let s0 = Rect(Point(-0.24,1.98,0.16),Point(-0.24,1.98,-0.22),Point(0.23,1.98,-0.22),Point(0.23,1.98,0.16), 5)
    let s1 = Rect(Point(-1.01,0.,0.99),Point(1.0,0.,0.99),Point(1.0,0.,-1.04),Point(-0.99,0.,-1.04), 0)
    let s2 = Rect(Point(-1.02,1.99,0.99),Point(-1.02,1.99,-1.04),Point(1.0,1.99,-1.04),Point(1.0,1.99,0.99), 1)
    let s3 = Rect(Point(-0.99,0.,-1.04),Point(1.,0.,-1.04),Point(1.,1.99,-1.04),Point(-1.02,1.99,-1.04), 2)
    let s4 = Rect(Point(1.,0.,-1.04),Point(1.,0.,0.99),Point(1.,1.99,0.99),Point(1.,1.99,-1.04),3)
    let s5 = Rect(Point(-1.01,0.,0.99),Point(-0.99,0.,-1.04),Point(-1.02,1.99,-1.04),Point(-1.02,1.99,0.99),4)
    let sb1 = Rect(Point(0.53,0.60,0.75),Point(0.70,0.60,0.17),Point(0.13,0.60,0.),Point(-0.05,0.6,0.57), 6)
    let sb2 = Rect(Point(-0.05,0.,0.57),Point(-0.05,0.6,0.57),Point(0.13,0.6,0.),Point(0.13,0.,0.), 6)
    let sb3 = Rect(Point(0.53,0.,0.75),Point(0.53,0.6,0.75),Point(-0.05,0.6,0.57),Point(-0.05,0.,0.57), 6)
    let sb4 = Rect(Point(0.7,0.,0.17),Point(0.7,0.6,0.17),Point(0.53,0.6,0.75),Point(0.53,0.,0.75), 6)
    let sb5 = Rect(Point(0.13,0.,0.),Point(0.13,0.6,0.),Point(0.7,0.6,0.17),Point(0.7,0.,0.17), 6)
    let sb6 = (*Bottom*) Rect(Point(0.53,0.,0.75),Point(0.7,0.,0.17),Point(0.13,0.,0.),Point(-0.05,0.,0.57), 6)
    let tb1 = Rect(Point(-0.53,1.2,0.09),Point(0.04,1.2,-0.09),Point(-0.14,1.2,-0.67),Point(-0.71,1.2,-0.49), 8)
    let tb2 = Rect(Point(-0.53,0.,0.09),Point(-0.53,1.2,0.09),Point(-0.71,1.2,-0.49),Point(-0.71,0.,-0.49), 8)
    let tb3 = Rect(Point(-0.71,0.,-0.49),Point(-0.71,1.2,-0.49),Point(-0.14,1.2,-0.67),Point(-0.14,0.,-0.67), 8)
    let tb4 = Rect(Point(-0.14,0.,-0.67),Point(-0.14,1.2,-0.67),Point(0.04,1.2,-0.09),Point(0.04,0.,-0.09), 8)
    let tb5 = Rect(Point(0.04,0.,-0.09),Point(0.04,1.2,-0.09),Point(-0.53,1.2,0.09),Point(-0.53,0.,0.09), 8)
    let tb6 = Rect(Point(-0.53,0.,0.09),Point(0.04,0.,-0.09),Point(-0.14,0.,-0.67),Point(-0.71,0.,-0.49), 8)
    let sph = NewSphere(Point(-0.6,0.25,0.6),0.25,9)

    [|s0;s1;s2;s3;s4;s5;sb1;sb2;sb3;sb4;sb5;sb6;tb1;tb2;tb3;tb4;tb5;tb6;sph;|]
    //[|s1;s2;s3;s4;s5;sb1;sb2;sb3;sb4;sb5;sb6;tb1;tb2;tb3;tb4;tb5;tb6;sph;|]

let Light = NewPointLight(Point(0,1.98,0), Color(0.78,0.78,0.78)):>INewLight
let AreaLight = NewAreaLight(Point(-0.24,1.98,0.16),Point(-0.24,1.98,-0.22),Point(0.23,1.98,-0.22),Point(0.23,1.98,0.16),Vector(0,-1,0),Color(10,10,10)):>INewLight

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
            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
            let ir = int (255.99*col.r)
            let ig = int (255.99*col.g)
            let ib = int (255.99*col.b)
            screen[i,(ny-1)-j] <- Color(ir,ig,ib), 1)

let DoRayTrace3() =
    let nx = 300
    let ny = 300
    let ns = 200

    let cam = new Camera(Point(0, 1, 3),Vector(0,0,-1), 120.0, 0.5, 1000.0, nx, ny)

    let screen = new Screen(nx, ny)
    let mat = new Mat(Size(nx, ny), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    let (w,h) = screen.Size()
    let arr = Array.allPairs [|0..w-1|] [|0..h-1|]

    let objs = GetObjects()
    //let objs = GetSpheres()
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