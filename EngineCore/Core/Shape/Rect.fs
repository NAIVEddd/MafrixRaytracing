module Engine.Core.Shapes.Rect
open Engine.Core.Shapes.Triangle
open Engine.Core.Interfaces.ISampler
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Aggregate
open Engine.Core.Point
open Engine.Core.Ray
open System

type Rect =
    struct
        val trig1 : Triangle
        val trig2 : Triangle
        val bound : Bound
        val area : float
        new(v0:Point,v1:Point,v2:Point,v3:Point,mat:int) =
            let t1 = Triangle(v0,v1,v2,mat)
            let t2 = Triangle(v0,v2,v3,mat)
            {
                trig1 = t1;
                trig2 = t2;
                bound = Bound.Union(t1.BoundBox(), t2.BoundBox())
                area = t1.Area() + t2.Area();
            }
        member this.Hit(r:Ray, tMin:float, tMax:float) =
            let h1 = this.trig1.Hit(r,tMin,tMax)
            if h1.hit then
                h1
            else
                this.trig2.Hit(r,tMin,tMax)
        member this.BoundBox() = this.bound
        member this.SamplePoint() =
            let s = Random.Shared.NextDouble()
            if s < 0.5 then
                this.trig1.SamplePoint()
            else
                this.trig2.SamplePoint()
        member this.Area() = this.area
        interface INewSamplable with
            member this.SamplePoint() = this.SamplePoint()
            member this.Area() = this.Area()
        interface IHitable with
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
            member this.BoundBox() = this.BoundBox()
    end

// type xy_rect =
//     struct
//         val x0:float
//         val x1:float
//         val y0:float
//         val y1:float
//         val k:float
//         val aabb:AABB
//         val material: IMaterial
//         new(_x0:float, _x1:float, _y0:float, _y1:float, _k:float, m:IMaterial) = 
//             let pmin = Vector(_x0,_y0,_k)
//             let pmax = Vector(_x1,_y1,_k)
//             {
//                 x0 = _x0; x1 = _x1;
//                 y0 = _y0; y1 = _y1;
//                 k = _k;
//                 aabb = AABB(pmin,pmax)
//                 material = m;
//             }
//         member this.ShadowHit(ray:Ray) = false, 0.0
//         member this.Hit(r:Ray, tMin:float, tMax:float) =
//             let t = (this.k - r.Origin().z) / r.Direction().z
//             if t < tMin || t > tMax then
//                 HitRecord.Nothing
//             else
//                 let x = r.Origin().x + t * r.Direction().x
//                 let y = r.Origin().y + t * r.Direction().y
//                 if x < this.x0 || x > this.x1 || y < this.y0 || y > this.y1 then
//                     HitRecord.Nothing
//                 else
//                     let p = r.PointAtParameter(t)
//                     HitRecord(true, t, p, Vector(0,0,1), r, Some this.material)
//         interface IHitable with
//             member this.BoundBox(t0:float,t1:float) = true, this.aabb
//             member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
//             member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
//     end

// type xz_rect =
//     struct
//         val x0:float
//         val x1:float
//         val z0:float
//         val z1:float
//         val k:float
//         val aabb:AABB
//         val material: IMaterial
//         new(_x0:float, _x1:float, _z0:float, _z1:float, _k:float, m:IMaterial) = 
//             let pmin = Vector(_x0,_k,_z0)
//             let pmax = Vector(_x1,_k,_z1)
//             {
//                 x0 = _x0; x1 = _x1;
//                 z0 = _z0; z1 = _z1;
//                 k = _k;
//                 aabb = AABB(pmin,pmax)
//                 material = m;
//             }
//         member this.ShadowHit(ray:Ray) = false, 0.0
//         member this.Hit(r:Ray, tMin:float, tMax:float) =
//             let t = (this.k - r.Origin().y) / r.Direction().y
//             if t < tMin || t > tMax then
//                 HitRecord.Nothing
//             else
//                 let x = r.Origin().x + t * r.Direction().x
//                 let z = r.Origin().z + t * r.Direction().z
//                 if x < this.x0 || x > this.x1 || z < this.z0 || z > this.z1 then
//                     HitRecord.Nothing
//                 else
//                     let p = r.PointAtParameter(t)
//                     HitRecord(true, t, p, Vector(0,1,0), r, Some this.material)
//         interface IHitable with
//             member this.BoundBox(t0:float,t1:float) = true, this.aabb
//             member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
//             member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
//     end

// type yz_rect =
//     struct
//         val y0:float
//         val y1:float
//         val z0:float
//         val z1:float
//         val k:float
//         val aabb:AABB
//         val material: IMaterial
//         new(_y0:float, _y1:float, _z0:float, _z1:float, _k:float, m:IMaterial) = 
//             let pmin = Vector(_k,_y0,_z0)
//             let pmax = Vector(_k,_y1,_z1)
//             {
//                 y0 = _y0; y1 = _y1;
//                 z0 = _z0; z1 = _z1;
//                 k = _k;
//                 aabb = AABB(pmin,pmax)
//                 material = m;
//             }
//         member this.ShadowHit(ray:Ray) = false, 0.0
//         member this.Hit(r:Ray, tMin:float, tMax:float) =
//             let t = (this.k - r.Origin().x) / r.Direction().x
//             if t < tMin || t > tMax then
//                 HitRecord.Nothing
//             else
//                 let y = r.Origin().y + t * r.Direction().y
//                 let z = r.Origin().z + t * r.Direction().z
//                 if y < this.y0 || y > this.y1 || z < this.z0 || z > this.z1 then
//                     HitRecord.Nothing
//                 else
//                     let p = r.PointAtParameter(t)
//                     HitRecord(true, t, p, Vector(1,0,0), r, Some this.material)
//         interface IHitable with
//             member this.BoundBox(t0:float,t1:float) = true, this.aabb
//             member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
//             member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
//     end

// type flip_normals =
//     struct
//         val ptr: IHitable
//         new(p:IHitable) = {ptr = p}
//         member this.ShadowHit(ray:Ray) = false, 0.0
//         member this.Hit(r:Ray, tMin:float, tMax:float) =
//             let hitRec = this.ptr.Hit(r, tMin, tMax)
//             if hitRec.bHit then
//                 HitRecord(true, hitRec.t, hitRec.p, -hitRec.normal, r, hitRec.material)
//             else
//                 hitRec
//         interface IHitable with
//             member this.BoundBox(t0:float,t1:float) = this.ptr.BoundBox(t0,t1)
//             member this.ShadowHit(ray:Ray) = this.ShadowHit(ray)
//             member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
//     end