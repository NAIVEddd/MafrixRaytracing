module Engine.Core.Shapes.Sphere
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Aggregate
open Engine.Core.Point
open Engine.Core.Ray

type Sphere =
    struct
        val center: Point
        val radius: float
        val material: IMaterial
        new(c:Point, r:float, mate) = {center = c; radius = r; material = mate}
        member this.ShadowHit(ray:Ray) = (this:>IHitable).ShadowHit(ray)
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitable).Hit(r,tMin,tMax)
        interface IHitable with
            member this.BoundBox(t0:float,t1:float) =
                let pmin = Vector(this.center.x,this.center.y,this.center.z)-Vector(this.radius,this.radius,this.radius)
                let pmax = Vector(this.center.x,this.center.y,this.center.z)+Vector(this.radius,this.radius,this.radius)
                true, AABB(pmin,pmax)
            member this.ShadowHit(ray:Ray) =
                let record = this.Hit(ray, 0.00001, 99999999.)
                if record.bHit then
                    true, record.t
                else
                    false, 0.0
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
                        HitRecord(true, tmp, p, (p-this.center)/this.radius, r, Some this.material)
                    else
                        let tmp = (-b + sqrt(discriminant))/(2.0*a)
                        if tmp < tMax && tmp > tMin then
                            let p = r.PointAtParameter(tmp)
                            HitRecord(true, tmp, p, (p-this.center)/this.radius, r, Some this.material)
                        else
                            HitRecord.Nothing
                else
                    HitRecord.Nothing
    end

type NewSphere =
    struct
        val center: Point
        val radius: float
        val bound : Bound
        val material: int
        new(c:Point, r:float, mate) =
            let v = Vector(r,r,r)
            {
                center = c; radius = r; material = mate;
                bound=Bound(c-v,c+v);
            }
        member this.Hit(r:Ray, tMin:float, tMax:float) =
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
                    NewHitRecord(tmin,p,(p-this.center).Normalize,r,this.material)
                elif tmax > tMin && tmax < tMax then
                    let p = r.PointAtParameter(tmax)
                    NewHitRecord(tmax,p,(p-this.center).Normalize,r,this.material)
                else
                    NewHitRecord.Empty
            else
                NewHitRecord.Empty
        member this.BoundBox() = this.bound
        interface INewHitable with
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
            member this.BoundBox() = this.BoundBox()
    end