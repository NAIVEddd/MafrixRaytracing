module RayTracing

open System
open Engine.Core.Color
open Engine.Core.Light
open Engine.Core.Point
open Engine.Core.Camera
open Engine.Core.RenderTarget
open Engine.Core.Pipeline
open Engine.Core.Transformation
open Engine.Core.Texture
open Engine.Model.Obj

type Ray(origin:Point, direc:Vector, ?ti:float) =
    let A = origin
    let B = direc.Normalize
    let time = defaultArg ti 0.0
    member this.Origin() = A
    member this.Direction() = B
    member this.Time() = time
    member this.PointAtParameter(t:float) = A + B*t
    
type AABB(a:Vector, b:Vector) =
    let pmin = a
    let pmax = b
    member this.min = pmin
    member this.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        [|0..2|] |> Array.forall(fun i ->
            let invD = 1.0/r.Direction()[i]
            let mutable t0 = min ((pmin[i] - r.Origin()[i])*invD) (pmax[i]-r.Origin()[i])*invD
            let mutable t1 = max ((pmin[i] - r.Origin()[i])*invD) (pmax[i]-r.Origin()[i])*invD
            if invD < 0. then
                let tmp = t0
                t0 <- t1
                t1 <- tmp
            let tmin = if t0 > tMin then t0 else tMin
            let tmax = if t1 < tMax then t1 else tMax
            tmin < tmax)

let SurroundingBox(box0:AABB, box1:AABB) =
    let small = Vector(min box0.min.x box1.min.x, min box0.min.y box1.min.y, min box0.min.z box1.min.z)
    let big = Vector(max box0.max.x box1.max.x, max box0.max.y box1.max.y, max box0.max.z box1.max.z)
    AABB(small,big)

[<AbstractClass>]
type Texture() =
    abstract member Value: float * float * Point -> Vector

type ConstantTexture(col:Color) =
    inherit Texture()
    override this.Value(u:float, v:float, p:Point) = Vector(col.r,col.g,col.b)

type CheckerTexture(even:Texture,odd:Texture) =
    inherit Texture()
    override this.Value(u:float, v:float, p:Point) =
        let sines = Math.Sin(10.*p.x)* Math.Sin(10.*p.y)*Math.Sin(10.*p.z)
        if sines < 0. then
            odd.Value(u,v,p)
        else
            even.Value(u,v,p)


let PerlinGenerate() =
    [|
        for _ in 0..255 do
            yield Random.Shared.NextDouble()
    |]
let Permute(p:int array) =
    for i = p.Length-1 downto 0 do
        let targ = int(Random.Shared.NextDouble()* float(i+1))
        let tmp = p[i]
        p[i] <- p[targ]
        p[targ] <- tmp
    p

let PerlinGeneratePerm() =
    let l = [|0..255|]
    Permute(l)

type Perlin() =
    static let ranfloat = PerlinGenerate()
    static let perm_x = PerlinGeneratePerm()
    static let perm_y = PerlinGeneratePerm()
    static let perm_z = PerlinGeneratePerm()
    member this.Noise(p:Point) =
        let u = p.x - Math.Floor(p.x)
        let v = p.y - Math.Floor(p.y)
        let w = p.z - Math.Floor(p.z)
        let i = int(4. * p.x) &&& 255
        let j = int(4. * p.y) &&& 255
        let k = int(4. * p.z) &&& 255
        let idx = perm_x[i] ^^^ perm_y[j] ^^^ perm_z[k]
        ranfloat[idx]

type NoiseTexture() =
    inherit Texture()
    let noise = Perlin()
    override this.Value(u,v,p) = Vector(1,1,1) * noise.Noise(p)

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
    abstract member BoundBox : t0:float * t1:float -> bool * AABB

type BvhNode =
    val left:IHitAble
    val right:IHitAble
    val box:AABB
    new(l,r,b) = {left=l;right=r;box=b}
    member this.BoundBox(t0:float,t1:float) =
        true, this.box
    member this.Hit(r:Ray, tMin:float, tMax:float) =
        let bhit = this.box.hit(r,tMin,tMax)
        if bhit = true then
            let hit_left,record_left = this.left.Hit(r,tMin,tMax)
            let hit_right,record_right = this.right.Hit(r,tMin,tMax)
            if hit_left && hit_right then
                if record_left.t < record_right.t then
                    true, record_left
                else
                    true, record_right
            elif hit_left then
                true, record_left
            elif hit_right then
                true, record_right
            else
                false, HitRecord.Nothing
        else
            (false, HitRecord.Nothing)
    interface IHitAble with
        member this.BoundBox(t0:float,t1:float) = this.BoundBox(t0,t1)
        member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)

let rec BuildBvhNode(l:IHitAble array, n:int, t0:float, t1:float) =
    let axis = int (3.0 * Random.Shared.NextDouble())
    let ls =
        if axis = 0 then
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.x)
        elif axis = 1 then
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.y)
        else
            l |> Array.sortBy (fun x ->
                let _,box = x.BoundBox(0,0)
                box.min.z)
    let left,right =
        if n = 1 then
            l[0],l[0]
        elif n = 2 then
            l[0],l[1]
        else
            let half = n / 2
            let l0,l1 = Array.take half ls, Array.skip half ls
            BuildBvhNode(l0,half,t0,t1),BuildBvhNode(l1,n-half,t0,t1)
    let _,bLeft = left.BoundBox(t0,t1)
    let _,bRight = right.BoundBox(t0,t1)
    let box = SurroundingBox(bLeft,bRight)
    BvhNode(left,right,box)

type Sphere =
    struct
        val center: Point
        val radius: float
        val material: Material
        new(c:Point, r:float, mate) = {center = c; radius = r; material = mate}
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitAble).Hit(r,tMin,tMax)
        interface IHitAble with
            member this.BoundBox(t0:float,t1:float) =
                let pmin = Vector(this.center.x,this.center.y,this.center.z)-Vector(this.radius,this.radius,this.radius)
                let pmax = Vector(this.center.x,this.center.y,this.center.z)+Vector(this.radius,this.radius,this.radius)
                true, AABB(pmin,pmax)
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

type MovingSphere =
    struct
        val center: Point
        val center1:Point
        val radius: float
        val time0:float
        val time1:float
        val material: Material
        new(cen0:Point, cen1:Point, t0:float, t1:float, r:float, mate) =
            {center = cen0;
            center1 = cen1;
            radius = r;
            time0 = t0;
            time1 = t1;
            material = mate}
        member this.Center(t:float) = this.center + (t-this.time0)/(this.time1-this.time0)*(this.center1-this.center)
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitAble).Hit(r,tMin,tMax)
        interface IHitAble with
            member this.BoundBox(t0:float,t1:float) =
                let cen = this.Center(t0)
                let pmin = Vector(this.center.x,this.center.y,this.center.z)-Vector(this.radius,this.radius,this.radius)
                let pmax = Vector(this.center.x,this.center.y,this.center.z)+Vector(this.radius,this.radius,this.radius)
                true, AABB(pmin,pmax)
            member this.Hit(r:Ray, tMin:float, tMax:float) =
                let oc = r.Origin() - this.Center(r.Time())
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
let Schlick(cosine:float, ref_idx:float) =
    let r0 = (1.-ref_idx)/(1.+ref_idx)
    let r1 = r0*r0
    r1+(1.-r1)*System.Math.Pow(1.-cosine, 5)

type Lambertian(a:Texture) =
    let albedo = a
    member this.Scatter(ray:Ray, hit:HitRecord) = (this:>Material).Scatter(ray,hit)
    interface Material with
        member this.Scatter(ray:Ray, hit:HitRecord) =
            let target = hit.normal.Normalize + GetRandomInUnitSphere()
            let scattered = Ray(hit.p, target)
            let col = a.Value(0,0,hit.p)
            true, col, scattered
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
            let mutable cosine = 0.0
            if ray.Direction().Dot(hit.normal) > 0 then
                outward_normal <- -hit.normal
                ni_over_nt <- ref_idx
                cosine <- ref_idx * ray.Direction().Dot(hit.normal)
            else
                outward_normal <- hit.normal
                ni_over_nt <- 1.0 / ref_idx
                cosine <- - ray.Direction().Dot(hit.normal)
            let (isreferact, ref_dir) = Refract(ray.Direction(), outward_normal, ni_over_nt)
            let reflect_prob =
                if isreferact then
                    Schlick(cosine,ref_idx)
                else
                    1.0
            if System.Random.Shared.NextDouble() < reflect_prob then
                true,Vector(1,1,1), Ray(hit.p, reflected)
            else
                true, Vector(1,1,1), Ray(hit.p, ref_dir)

let RandomInUnitDisk() =
    let mutable p = Vector()
    let mutable dot = 1.0
    while dot >= 1.0 do
        p <- 2.0*Vector(Random.Shared.NextDouble(), Random.Shared.NextDouble(), 0) - Vector(1,1,0)
        dot <- p.Dot(p)
    p

type RayTraceCamera(lookfrom:Point, lookat:Point, vup:Vector, vfov:float, aspect:float, aperture:float, focus_dist:float, t0:float, t1:float) =
    let mutable origin=Point()
    let mutable lowerLeftCorner=Vector()
    let mutable horizontal=Vector()
    let mutable vertical = Vector()
    let mutable lensRadius = 0.0
    let mutable w = Vector()
    let mutable u = Vector()
    let mutable v = Vector()
    let mutable time0 = t0
    let mutable time1 = t1
    do
        lensRadius<-aperture/2.0
        let theta = vfov*Math.PI/180.
        let half_height = Math.Tan(theta/2.)
        let half_width = aspect * half_height
        origin<-lookfrom
        w <- (lookfrom-lookat).Normalize
        u <- vup.Cross(w).Normalize
        v <- w.Cross(u).Normalize
        let p = origin - focus_dist*half_width*u - focus_dist*half_height*v - focus_dist*w
        lowerLeftCorner<- Vector(p.x,p.y,p.z)
        horizontal<-2.* focus_dist *half_width*u
        vertical<-2.* focus_dist *half_height*v
    
    member this.GetRay(s:float, t:float) =
        let rd = lensRadius*RandomInUnitDisk()
        let offset = u*rd.x + v*rd.y
        let time = time0 + Random.Shared.NextDouble() * (time1-time0)
        Ray(origin+offset, lowerLeftCorner + s*horizontal + t*vertical - Vector(origin.x,origin.y,origin.z) - offset, time)


let rec GetColor(ray:Ray, hitable:IHitAble[], depth:int) =
    let (ishit, hitrecord) = ListHit(hitable, ray, 0.00001, 10000000)
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

let RandomScene(rand:Random) :IHitAble[] =
    let n = 500
    let arr = Array.allPairs [|-1..11|] [|-1..11|]
    let checker = CheckerTexture(ConstantTexture(Color(0.2,0.3,0.1)), ConstantTexture(Color(0.9,0.9,0.9)))
    let pertext = NoiseTexture()
    Array.append
        [|
            for a,b in arr do
                let choose_mat = rand.NextDouble()
                let center = Vector(float a+0.9*rand.NextDouble(),0.2,float b+rand.NextDouble())
                let cen = Point(center.x,center.y,center.z)
                if (center-Vector(4,0.2,0)).Length > 0.9 then
                    if choose_mat < 0.8 then
                        let a = rand.NextDouble()*rand.NextDouble()
                        let b = rand.NextDouble()*rand.NextDouble()
                        let c = rand.NextDouble()*rand.NextDouble()
                        let tex = ConstantTexture(Color(a,b,c))
                        yield Sphere(cen, 0.2, Lambertian(tex))
                    elif choose_mat < 0.95 then
                        let vec = Vector(0.5*(1.+rand.NextDouble()), 0.5*(1.+rand.NextDouble()), 0.5*(1.+rand.NextDouble()))
                        yield Sphere(cen, 0.2, Metal(vec, 0.5*rand.NextDouble()))
                    else
                        yield Sphere(cen, 0.2, Dielectric(1.5))
        |]
        [|
            Sphere(Point(0,-1000,0),1000, Lambertian(pertext))
            //Sphere(Point(0,-1000,0),1000, Lambertian(checker))
            //Sphere(Point(0,-1000,0),1000, Lambertian(ConstantTexture(Color(0.5,0.5,0.5))))
            Sphere(Point(0,1,0),1.0,Dielectric(1.5))
            Sphere(Point(-4,1,0),1.0,Lambertian(ConstantTexture(Color(0.4,0.2,0.1))))
            Sphere(Point(4,1,0),1.0,Metal(Vector(0.7,0.6,0.5),0.0))
        |]

let DoRayTrace() =
    let lower_left_corner = Vector(-2.0, -1.0, -1.0)
    let horizontal = Vector(4.0, 0.0, 0.0)
    let vertical = Vector(0.0, 2.0, 0.0)
    let origin = Point()

    let nx = 400
    let ny = 200
    let ns = 9
    let rand = new System.Random()

    let lookfrom = Point(13,2,3)
    let lookat = Point(0,0,0)
    let vup = Vector(0,1,0)
    let dist_to_focus = (lookfrom-lookat).Length
    let aperture = 0.
    let cam = new RayTraceCamera(lookfrom,lookat,vup,20,float nx/float ny, aperture, dist_to_focus,0.,1.)
    //let hitableList = [|Sphere(Point(0, 0, -1), 0.5, Lambertian(Vector(0.8,0.3,0.3))):>IHitAble;
    //                    Sphere(Point(0,-100.5,-1), 100, Lambertian(Vector(0.8,0.8,0.0)))
    //                    Sphere(Point(-1,0,-1), 0.5, Dielectric(1.5))
    //                    Sphere(Point(1,0,-1), 0.5, Metal(Vector(0.8,0.6,0.2), 0.3))
    //                    |]
    let hitableList = RandomScene(rand)

    let screen = new Screen(nx, ny)
    //let mat = new Mat(Size(nx, ny), MatType.CV_8UC3)
    //let indexer = mat.GetGenericIndexer<Vec3b>()
    //let (w,h) = screen.Size()
    //let arr = Array.allPairs [|0..w-1|] [|0..h-1|]
    //arr |> Array.map(fun (i,j) -> 
    //        async{
    //            let mutable col = Color()
    //            for s = 0 to ns-1 do
    //                let u = (float i + rand.NextDouble()) / float nx
    //                let v = (float j + rand.NextDouble()) / float ny
    //                let ray = cam.GetRay(u,v)
    //                let c = GetColor(ray, hitableList, 0)
    //                col <- col + c
    //            col <- col / float ns
    //            col <- Color(sqrt(col.r), sqrt(col.g), sqrt(col.b))
    //            let ir = int (255.99*col.r)
    //            let ig = int (255.99*col.g)
    //            let ib = int (255.99*col.b)
    //            screen[i,(ny-1)-j] <- Color(ir,ig,ib), 1
    //        })
    //    |> Async.Parallel
    //    |> Async.Ignore
    //    |> Async.RunSynchronously


    //arr |> Array.map(fun(x,y) ->
    //    let c = screen.Pixel(x,y)
    //    let vec = Vec3b(byte c.b, byte c.g, byte c.r)
    //    indexer[y,x] <- vec) |> ignore
            
    //Cv2.ImShow("Manga", mat)
    //Cv2.WaitKey() |> ignore
    ()