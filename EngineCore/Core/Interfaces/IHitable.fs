module Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Ray
open Engine.Core.Point

type AABB(a:Vector, b:Vector) =
    let pmin = a
    let pmax = b
    member this.min = pmin
    member this.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        let mutable bHit = true
        for i in 0..2 do
            let invD = 1.0/r.Direction()[i]
            let mutable t0 = ((pmin[i] - r.Origin()[i])*invD)
            let mutable t1 = ((pmax[i] - r.Origin()[i])*invD)
            if invD < 0. then
                let tmp = t0
                t0 <- t1
                t1 <- tmp
            let tmin = if t0 > tMin then t0 else tMin
            let tmax = if t1 < tMax then t1 else tMax
            bHit <- bHit && tmin < tmax
        bHit

type IHitable =
    abstract member Hit : r:Ray * tMin:float * tMax:float -> HitRecordT<IMaterial>
    abstract member ShadowHit : r:Ray -> bool * distance:float
    abstract member BoundBox : t0:float * t1:float -> bool * AABB