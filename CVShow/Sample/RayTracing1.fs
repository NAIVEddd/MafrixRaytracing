module RayTracing1

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
open Engine.Core.Shapes.CircleAreaLightObject
open Engine.Core.Samplers.JitteredSampler
open Engine.Core.Materials.Lambertian
open Engine.Core.Materials.GlossySpecular
open Engine.Core.Materials.PerfectSpecular

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

let ListHit(items:IHitable[], r:Ray, tmin:float, tmax:float) =
    items |> Array.map(fun i -> i.Hit(r,tmin,tmax)) |> Array.minBy(fun (ishit, hitrecord) -> (
        if ishit then hitrecord.t else tmax))

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

type World(_items:IHitable[], amb:ILight, _lights:ILight[]) =
    let mutable items = _items
    let mutable lights = _lights
    let mutable tracer = Unchecked.defaultof<ITracer>
    interface IWorld with
        member this.ShadowHit(ray:Ray) =
            let bHit, record = (this:IHitable).Hit(ray, 0.00001, 99999999.0)
            if bHit then
                bHit, record.t
            else
                false, 0.0
        member this.Hit(r:Ray, tMin:float, tMax:float) =
            items |> Array.map(fun i -> i.Hit(r,tMin,tMax)) |> Array.minBy(fun (ishit, hitrecord) -> (
                if ishit then hitrecord.t else tMax))
        member this.GetAmbientLight() = amb
        member this.GetLights() = lights
        member this.AddLight(light) = lights <- Array.append lights [|light|]
        member this.GetObjects() = items
        member this.AddObject(obj) = items <- Array.append items [|obj|]
        member this.SetTracer(t) = tracer <- t
        member this.GetTracer() = tracer

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

type PhongArea(ka:float, kd:float, ks:float, spec:float, color:Color) =
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
            let mutable l = Color()
            for light in (world.GetLights()) do
                for _ in [0..63] do
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
                l <- l / 64.0
            l <- l + ambient_brdf.rho(wo, 1,1) * world.GetAmbientLight().L(hit)
            l

type Emissive(ls:float, normal:Vector, color:Color) =
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>IMaterial).Scatter(ray,hit)
    interface IMaterial with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            false,Vector(),Ray()
        member this.Shade(hit,w,depth,wo) = (this:>IMaterial).Shade(hit,w)
        member this.PathShade(hit, w, depth) = Color()
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
            let col = reflective_brdf.Sample_f(hit, ref wi, -ray.Direction(), 0,0,ref pdf)
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
            let pdf = 0.0
            let col = reflective_brdf.Sample_f(hit, &wi, -hit.hitRay.Direction(), 0,0,ref pdf)
            let scattered = Ray(hit.p, wi)// + GetRandomInUnitSphere())
            0.5 * phong.Shade(hit,w) + 0.5 * col * hit.normal.Dot(wi) * tracer.TraceRay(scattered, depth+1)
        member this.Shade(hit, w:obj) =
            tracer.TraceRay(hit.hitRay, 0)

type RayTraceCamera() =
    let mutable origin = Point()
    let mutable lowerLeftCorner = Vector(-2,-1,-1)
    let mutable horizontal = Vector(4,0,0)
    let mutable vertical = Vector(0.0, 2.0, 0.0)
    
    member this.GetRay(u:float, v:float) = Ray(origin, (lowerLeftCorner + u*horizontal + v*vertical).Normalize)


let rec GetColor(ray:Ray, hitable:IHitable[], depth:int) =
    let (ishit, hitrecord) = ListHit(hitable, ray, 0, 10000000)
    if ishit then
        let n = hitrecord.normal.Normalize
        let targ = n + GetRandomInUnitSphere()
        let (ishit, attenuation, scattered) = hitrecord.material.Value.Scatter(ray, hitrecord)
        if depth < 25 && ishit then
            let c = GetColor(scattered, hitable, depth + 1)
            Color(c.r*attenuation.x, c.g*attenuation.y, c.b*attenuation.z)
        else
            Color()
    else
        let unitDirection = ray.Direction().Normalize
        let t = 0.5 * (unitDirection.y + 1.0)
        let vec = (1.0-t)*Vector(1,1,1) + t*Vector(0.5,0.7,1.0)
        Color(vec.x, vec.y, vec.z)


let DoRayTrace1() =
    let nx = 400
    let ny = 200
    let ns = 9
    let samp = JitteredSampler(ns)
    samp.GenerateSamples()
    let cam = new RayTraceCamera()
    //let hitableList = [|Sphere(Point(0, 0, -1), 0.5, Lambertian(Vector(0.8,0.3,0.3))):>IHitable;
    //                    Sphere(Point(0,-100.5,-1), 100, Lambertian(Vector(0.8,0.8,0.0)))
    //                    Sphere(Point(-1,0,-1), 0.5, Dielectric(1.5))
    //                    Sphere(Point(1,0,-1), 0.5, Matte(0.25,0.75,Color(0.8,0.6,0.2)))|]
                        //Sphere(Point(1,0,-1), 0.5, Metal(Vector(0.8,0.6,0.2), 0.3))|]
    let hitableList:IHitable[] = [|Sphere(Point(0,0,-1), 0.5, PhongArea(0.15,0.65,0.2,10,Color(0.8,0.6,0.2)))
                                   Sphere(Point(0,-100.5,-1), 100, PhongArea(0.15,0.65,0.2,10,Color(0.8,0.8,0.0)))
        |]
    let lights = [|PointLight(3,Color(1,1,1),Point(10,10,5)):>ILight
                   //PointLight(1,Color(1,1,1),Point(-10,10,0))
        |]
    
    let ambientlight = Ambient()
    let world = World([||], ambientlight, [||]) :> IWorld
    let alObj = CircleAreaLightObject(Point(0,2.5,-5), 3, Vector(0,0,1),Emissive(0.98,Vector(0,0,1),Color(1,1,1)))
    world.AddObject(alObj)
    world.AddLight(AreaLight(0.95,3,Color(1,1,1), Point(0,2.5,-5),Vector(0,0,1),alObj,world))
    
    let tracer = RayCast(world)
    world.SetTracer(tracer)
    let refle = Reflective(Vector(0.8,0.6,0.2),0.5,Color(0.8,0.6,0.2),Whitted(world,5))
    world.AddObject(Sphere(Point(1,0,-1), 0.5, refle))
    world.AddLight(PointLight(3,Color(1,1,1),Point(10,10,5)))
    //world.AddLight(PointLight(1,Color(1,1,1),Point(-10,10,0)))
    world.AddObject(Sphere(Point(0,0,-1), 0.5, PhongArea(0.15,0.65,0.2,10,Color(0.8,0.6,0.2))))
    world.AddObject(Sphere(Point(0,-100.5,-1), 100, PhongArea(0.15,0.65,0.2,10,Color(0.8,0.8,0.0))))

    // path trace
    //let tracer = PathTracer(world,5)
    //world.SetTracer(tracer)
    //let obj1 = Sphere(Point(0,0,-1), 0.5, Matte(0.25,0.75,Color(0.8,0.6,0.2)))
    //world.AddObject(obj1)
    //let obj2 = Sphere(Point(0,-100.5,-1), 100, Matte(0.25,0.75,Color(0.8,0.8,0)))
    //world.AddObject(obj2)

    let screen = new Screen(nx, ny)
    let mat = new Mat(Size(nx, ny), MatType.CV_8UC3)
    let indexer = mat.GetGenericIndexer<Vec3b>()
    let (w,h) = screen.Size()
    let arr = Array.allPairs [|0..w-1|] [|0..h-1|]
    arr |> Array.map(fun (i,j) -> 
            async{
                let mutable col = Color()
                for s = 0 to ns-1 do
                    let sp = samp.SampleUnitSquare()
                    let u = (float i + sp.x) / float nx
                    let v = (float j + sp.y) / float ny
                    let ray = cam.GetRay(u,v)
                    let c = tracer.TraceRay(ray, 0)
                    //let c = GetColor(ray, hitableList, 0)
                    col <- col + c
                col <- col / float ns
                col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
                let ir = int (255.99*col.r)
                let ig = int (255.99*col.g)
                let ib = int (255.99*col.b)
                screen[i,(ny-1)-j] <- Color(ir,ig,ib), 1
            })
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously


    arr |> Array.map(fun(x,y) ->
        let c = screen.Pixel(x,y)
        let vec = Vec3b(byte c.b, byte c.g, byte c.r)
        indexer[y,x] <- vec) |> ignore
            
    Cv2.ImShow("Manga", mat)
    Cv2.WaitKey() |> ignore