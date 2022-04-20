module RayTracing2

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

let GetRandomInUnitSphere() =
    let mutable p = Vector(20,20,20)
    let rand = new System.Random()
    while p.Dot(p) >= 1.0 do
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
        (false, Vector())


type Lambertian(a:Vector) =
    let albedo = a
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let target = hit.normal.Normalize + GetRandomInUnitSphere()
            let scattered = Ray(hit.p, target)
            true, albedo, scattered
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.Shade(hit, w) = Color()
        member this.PathShade(hit, w, depth) = Color()
type Metal(a:Vector, f:float) =
    let albedo = a
    let fuzz = if f < 1.0 then f else 1.0
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let reflected = Reflect(ray.Direction().Normalize, hit.normal)
            let scattered = Ray(hit.p, reflected + fuzz*GetRandomInUnitSphere())
            scattered.Direction().Dot(hit.normal) > 0, albedo, scattered
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.Shade(hit, w) = Color(a.x,a.y,a.z)
        member this.PathShade(hit, w, depth) = Color()
type Dielectric(ri:float) =
    let ref_idx = ri
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let reflected = Reflect(ray.Direction(), hit.normal)
            let mutable outward_normal = Vector()
            let mutable ni_over_nt = 0.0
            if ray.Direction().Dot(hit.normal) > 0 then
                outward_normal <- -hit.normal
                ni_over_nt <- ref_idx
            else
                outward_normal <- hit.normal
                ni_over_nt <- 1.0 / ref_idx
            let (isreferact, ref_dir) = Refract(ray.Direction(), outward_normal, ni_over_nt)
            if isreferact then
                true, Vector(1,1,1), Ray(hit.p, ref_dir)
            else
                true,Vector(1,1,1), Ray(hit.p, reflected)
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.Shade(hit, w) = Color()
        member this.PathShade(hit, w, depth) = Color()


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
            //let rand = System.Random.Shared
            //if rand.NextDouble() < 1. then
            //    let t = world.GetTracer().TraceRay(reflected, depth+1)
            //    f * t * ndotwi / pdf
            //else
            //    let on_light = Point(213. + rand.NextDouble() * (343.-213.), 554, 227. + rand.NextDouble() * (332.-227.))
            //    let to_light = on_light - hit.p
            //    let distance_squared = to_light.LengthSquare
            //    let unit_to_light = to_light.Normalize
            //    if unit_to_light.Dot(hit.normal) < 0. then
            //        Color()
            //    else
            //        let light_area = (343.-213.)*(332.-227.)
            //        let light_cosine = System.Math.Abs(unit_to_light.y)
            //        if light_cosine < 0.00001 then
            //            Color()
            //        else
            //            pdf <- distance_squared / (light_cosine * light_area)
            //            let scattered = Ray(hit.p, unit_to_light)
            //            let t = world.GetTracer().TraceRay(scattered, depth+1)
            //            f * t / pdf
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
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            false,Vector(),Ray()
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.PathShade(hit, w, depth) =
            let dot = (-hit.normal).Dot(hit.hitRay.Direction())
            if dot >= 0.0 then
                ls * color
            else
                Color()
        member this.Shade(hit, w:obj) =
            let dot = (-hit.normal).Dot(hit.hitRay.Direction())
            if dot >= 0.0 then
                ls * color
            else
                Color()

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
    let r3 = flip_normals(xz_rect(213, 343, 227, 323, 554, light))
    let r4 = xz_rect(0, 555, 0, 555, 0, white)
    let r5 = flip_normals(xy_rect(0, 555, 0, 555, 555, white))
    let r6 = flip_normals(xz_rect(0, 555, 0, 555, 555, white))
    let b1 = Translate(Rotate_y(Box(Vector(0, 0, 0), Vector(165, 165, 165), white), -18), Vector(130, 0, 65))
    let b2 = Translate(Rotate_y(Box(Vector(0, 0, 0), Vector(165, 330, 165), white), 15), Vector(265, 0, 295))
    [|r1;r2;r3;r4;r5;r6;b1;b2|]


let ThreadCount = 8
let DrawScreenByChunk(cam:Camera, tracer:Tracer, screen:Screen, arr:(int*int) array, chunkPerThread:int, samplePerPixel:int) =
    let rand = new System.Random()
    let swap x y (a: 'a []) =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let ns = samplePerPixel
    let ny = cam.height
    //arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
    arr |> Array.chunkBySize chunkPerThread |>
        Array.map(fun pixels ->
            async{
                let samp = JitteredSampler(ns)
                samp.GenerateSamples()
                let colors = pixels |> Array.map(fun (i,j) -> 
                    let mutable col = Color()
                    for s = 0 to ns-1 do
                        let sp = samp.SampleUnitSquare()
                        //let u = (float i + sp.x) / float nx
                        //let v = (float j + sp.y) / float ny
                        let u = float i + sp.x
                        let v = float j + sp.y
                        let ray = cam.GetRay(u, v)
                        //let ray = cam.GetRay(u,v)
                        let c = tracer.TraceRay(ray, 0)
                        //let c = GetColor(ray, hitableList, 0)
                        col <- col + c
                    col <- col / float ns
                    col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
                    let ir = int (255.99*col.r)
                    let ig = int (255.99*col.g)
                    let ib = int (255.99*col.b)
                    Color(ir,ig,ib), 1.
                )
                pixels |> Array.iteri (fun idx (i,j) -> screen[i,(ny-1)-j] <- colors[idx])
            })
        |> function(l) -> l,ThreadCount// 16 thread
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously

let DrawScreenByPixel(cam:Camera, tracer:Tracer, screen:Screen, arr:(int*int) array, samplePerPixel:int) =
    let ns = samplePerPixel
    let ny = cam.height
    let samp = JitteredSampler(ns)
    samp.GenerateSamples()
    //arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
    arr |> Array.map(fun (i,j) -> 
        async{
            let mutable col = Color()
            for s = 0 to ns-1 do
                let sp = samp.SampleUnitSquare()
                //let u = (float i + sp.x) / float nx
                //let v = (float j + sp.y) / float ny
                let u = float i + sp.x
                let v = float j + sp.y
                let ray = cam.GetRay(u, v)
                //let ray = cam.GetRay(u,v)
                let c = tracer.TraceRay(ray, 0)
                //let c = GetColor(ray, hitableList, 0)
                col <- col + c
            col <- col / float ns
            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
            let ir = int (255.99*col.r)
            let ig = int (255.99*col.g)
            let ib = int (255.99*col.b)
            screen[i,(ny-1)-j] <- Color(ir,ig,ib), 1})
        |> function(l) -> l,ThreadCount// 16 thread
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously



let DoRayTrace2() =
    let ChunkPerThread = 256
    let nx = 300
    let ny = 300
    let ns = 256

    
    let ambientlight = Ambient()
    let world = World([||], ambientlight, [||]) :> IWorld

    let tracer = PathTracer(world, 15)
    world.SetTracer(tracer)
    let refle = Reflective(Vector(0.8,0.6,0.2),0.5,Color(0.8,0.6,0.2),tracer)
    //world.AddObject(Sphere(Point(1,0,-1), 0.5, refle))
    for o in GetCornellBox() do
        world.AddObject(o)

    let obj1 = Sphere(Point(0,0,-1), 0.5, Matte(0.25,0.75,Color(0.8,0.6,0.2)))
    let obj3 = Sphere(Point(0,0,-1), 0.5, refle)
    world.Build()

    let cam = new Camera(Point(278, 278, -500),Vector(0,0,1), 120.0, 0.5, 1000.0, nx, ny)

    let screen = new Screen(nx, ny)
    let mat = new Mat(Size(nx, ny), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    let (w,h) = screen.Size()
    let arr = Array.allPairs [|0..w-1|] [|0..h-1|]
    let RenderWithContinus() =
        let mutable nsCount = 0.
        let mutable key = 0
        while key <> int 'q' do
            let tmpScreen = new Screen(nx, ny)
    
        // random shuffer pixels
        //arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
            DrawScreenByChunk(cam, tracer, tmpScreen, arr, ChunkPerThread, 1)
        //DrawScreenByPixel(cam, tracer, screen, arr, ns)
            nsCount <- nsCount + 1.
            arr |> Array.iter(fun(x,y) ->
                let oneC = tmpScreen.Pixel(x,y)
                let oldC = screen.Pixel(x,y)
                let newC = (oneC + oldC) / 2.
                screen[x,y] <- newC, 1.
                let c = newC
                let vec = Vec3b(byte c.b, byte c.g, byte c.r)
                indexer[y,x] <- vec)
            Cv2.ImShow("Manga", mat)
            key <- Cv2.WaitKey(1)

    let RenderWithSingleFrame() =
        //random shuffer pixels
        let rand = new System.Random()
        let swap x y (a: 'a []) =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp
        arr |> Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr)))
        DrawScreenByChunk(cam, tracer, screen, arr, ChunkPerThread, ns)
        //DrawScreenByPixel(cam, tracer, screen, arr, ns)
        arr |> Array.iter(fun(x,y) ->
            let c = screen.Pixel(x,y)
            let vec = Vec3b(byte c.b, byte c.g, byte c.r)
            indexer[y,x] <- vec)
            
        Cv2.ImShow("Manga", mat)
        Cv2.WaitKey() |> ignore

    //RenderWithContinus()
    RenderWithSingleFrame()