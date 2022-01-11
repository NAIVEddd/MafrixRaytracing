module Engine.Core.Shapes.CircleAreaLightObject
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Shapes.Sphere
open Engine.Core.Point
open Engine.Core.Ray

type CircleAreaLightObject =
    struct
        val sphere:Sphere
        val normal: Vector
        val material: IMaterial
        new(c:Point, r:float, nm:Vector, mate) = { sphere = Sphere(c,r,mate); normal = nm; material = mate}
        member this.Hit(r:Ray, tMin:float, tMax:float) = (this:>IHitable).Hit(r,tMin,tMax)
        interface IHitable with
            member this.ShadowHit(ray:Ray) =
                false, 0.0
            member this.Hit(r:Ray, tMin:float, tMax:float) =
                let bHit, record = this.sphere.Hit(r,tMin,tMax)
                if bHit then
                    bHit, HitRecord(bHit, record.t, record.p, this.normal, record.hitRay, record.material)
                else
                    bHit, record
    end