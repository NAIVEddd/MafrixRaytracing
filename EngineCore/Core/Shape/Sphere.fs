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
                    HitRecord(tmin,p,(p-this.center).Normalize,r,this.material)
                elif tmax > tMin && tmax < tMax then
                    let p = r.PointAtParameter(tmax)
                    HitRecord(tmax,p,(p-this.center).Normalize,r,this.material)
                else
                    HitRecord.Empty
            else
                HitRecord.Empty
        member this.BoundBox() = this.bound
        interface IHitable with
            member this.Hit(r:Ray, tMin:float, tMax:float) = this.Hit(r,tMin,tMax)
            member this.BoundBox() = this.BoundBox()
    end