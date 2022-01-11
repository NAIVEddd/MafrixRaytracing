module RayTracing

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
open Engine.Model.Obj

type Ray(origin:Point, direc:Vector) =
    let A = origin
    let B = direc
    member this.Origin() = A
    member this.Direction() = B
    member this.PointAtParameter(t:float) = A + B*t

type HitRecord =
    struct
        val t: float
        val p: Point
        val normal: Vector
        val material: Option<Material>
        new(_t:float, _p:Point, _nm:Vector, mate:Option<Material>) = {t = _t; p = _p; normal = _nm; material = mate}
        static member Nothing = HitRecord(-1,Point(),Vector(), None)
    end

and Material =
    abstract member Scatter : ray:Ray * HitRecord -> bool * attenuation:Vector * Ray

type IHitAble =
    abstract member Hit : r:Ray * tMin:float * tMax:float -> bool * HitRecord

type Sphere =
    struct
        val center: Point
        val radius: float
        val material: Material
        new(c:Point, r:float, mate) = {center = c; radius = r; material = mate}
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitAble).Hit(r,tMin,tMax)
        interface IHitAble with
            member this.Hit(r:Ray, tMin:float, tMax:float) =
                let oc = r.Origin() - this.center
                let a = r.Direction().Dot(r.Direction())
                let b = 2.0 * oc.Dot(r.Direction())
                let c = oc.Dot(oc) - this.radius*this.radius
                let discriminant = b*b-4.0*a*c
                if discriminant > 0 then
                    let tmp = (-b - sqrt(discriminant))/(2.0*a)
                    if tmp < tMax && tmp > tMin then
                        let p = r.PointAtParameter(tmp)
                        (true, HitRecord(tmp, p, (p-this.center)/this.radius, Some this.material))
                    else
                        let tmp = (-b + sqrt(discriminant))/(2.0*a)
                        if tmp < tMax && tmp > tMin then
                            let p = r.PointAtParameter(tmp)
                            (true, HitRecord(tmp, p, (p-this.center)/this.radius, Some this.material))
                        else
                            (false, HitRecord.Nothing)
                else
                    (false, HitRecord.Nothing)
    end

let ListHit(items:IHitAble[], r:Ray, tmin:float, tmax:float) =
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
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>Material).Scatter(ray,hit)
    interface Material with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let target = hit.normal.Normalize + GetRandomInUnitSphere()
            let scattered = Ray(hit.p, target)
            true, albedo, scattered
type Metal(a:Vector, f:float) =
    let albedo = a
    let fuzz = if f < 1.0 then f else 1.0
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>Material).Scatter(ray,hit)
    interface Material with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let reflected = Reflect(ray.Direction().Normalize, hit.normal)
            let scattered = Ray(hit.p, reflected + fuzz*GetRandomInUnitSphere())
            scattered.Direction().Dot(hit.normal) > 0, albedo, scattered
type Dielectric(ri:float) =
    let ref_idx = ri
    interface Material with
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


type RayTraceCamera() =
    let mutable origin = Point()
    let mutable lowerLeftCorner = Vector(-2,-1,-1)
    let mutable horizontal = Vector(4,0,0)
    let mutable vertical = Vector(0.0, 2.0, 0.0)
    
    member this.GetRay(u:float, v:float) = Ray(origin, lowerLeftCorner + u*horizontal + v*vertical)


let rec GetColor(ray:Ray, hitable:IHitAble[], depth:int) =
    let (ishit, hitrecord) = ListHit(hitable, ray, 0, 10000000)
    if ishit then
        let n = hitrecord.normal.Normalize
        let targ = n + GetRandomInUnitSphere()
        let (ishit, attenuation, scattered) = hitrecord.material.Value.Scatter(ray, hitrecord)
        if depth < 50 && ishit then
            let c = GetColor(scattered, hitable, depth + 1)
            Color(c.r*attenuation.x, c.g*attenuation.y, c.b*attenuation.z)
        else
            Color()
    else
        let unitDirection = ray.Direction().Normalize
        let t = 0.5 * (unitDirection.y + 1.0)
        let vec = (1.0-t)*Vector(1,1,1) + t*Vector(0.5,0.7,1.0)
        Color(vec.x, vec.y, vec.z)

let DoRayTrace() =
    use file = new StreamWriter("./RayTracing.ppm", false)

    let lower_left_corner = Vector(-2.0, -1.0, -1.0)
    let horizontal = Vector(4.0, 0.0, 0.0)
    let vertical = Vector(0.0, 2.0, 0.0)
    let origin = Point()

    let nx = 800
    let ny = 400
    let ns = 100
    let rand = new System.Random()
    file.WriteLine((sprintf "P3\n%A %A\n255" nx ny))

    let cam = new RayTraceCamera()
    let hitableList = [|Sphere(Point(0, 0, -1), 0.5, Lambertian(Vector(0.8,0.3,0.3))):>IHitAble;
                        Sphere(Point(0,-100.5,-1), 100, Lambertian(Vector(0.8,0.8,0.0)))
                        Sphere(Point(-1,0,-1), 0.5, Dielectric(1.5))
                        Sphere(Point(1,0,-1), 0.5, Metal(Vector(0.8,0.6,0.2), 0.3))|]
    for j = ny-1 downto 0 do
        for i = 0 to nx-1 do
            let mutable col = Color()
            for s = 0 to ns-1 do
                let u = (float i + rand.NextDouble()) / float nx
                let v = (float j + rand.NextDouble()) / float ny
                let ray = cam.GetRay(u,v)
                let c = GetColor(ray, hitableList, 0)
                col <- col + c
            col <- col / float ns
            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
            let ir = int (255.99*col.r)
            let ig = int (255.99*col.g)
            let ib = int (255.99*col.b)
            file.WriteLine((sprintf "%A %A %A" ir ig ib))