module Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Aggregate
open Engine.Core.Ray
open Engine.Core.Point

type AABB(a:Vector, b:Vector) =
    let pmin = a
    let pmax = b
    static let RIGHT = 0
    static let LEFT = 1
    static let MIDDLE = 2
    member this.min = pmin
    member this.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        // Code rewrite from:
        // https://github.com/erich666/GraphicsGems/blob/master/gems/RayBox.c
        let mutable inside = true
        let quadrant = Array.zeroCreate 3
        let candidatePlane = Array.zeroCreate<float> 3
        let origin = r.Origin()
        let dir = r.Direction()
        for i in 0..2 do
            if origin[i] < this.min[i] then
                quadrant[i] <- LEFT
                candidatePlane[i] <- this.min[i]
                inside <- false
            elif origin[i] > this.max[i] then
                quadrant[i] <- RIGHT
                candidatePlane[i] <- this.max[i]
                inside <- false
            else
                quadrant[i] <- MIDDLE
        if inside then
            true
        else
            let maxT = Array.zeroCreate<float> 3
            for i in 0..2 do
                if quadrant[i] <> MIDDLE && abs(dir[i]) > 1e-6 then
                    maxT[i] <- (candidatePlane[i]-origin[i])/dir[i]
                else
                    maxT[i] <- -1.

            let mutable whichPlane = 0
            for i in 1..2 do
                if maxT[whichPlane] < maxT[i] then
                    whichPlane <- i
            if maxT[whichPlane] < 0. then
                false
            else
                let mutable result = true
                for i in 0..2 do
                    if whichPlane <> i then
                        let coord = origin[i] + maxT[whichPlane] * dir[i]
                        if coord < this.min[i] || coord > this.max[i] then
                            result <- result && false
                result

type IHitable =
    abstract member Hit : r:Ray * tMin:float * tMax:float -> HitRecordT<IMaterial>
    abstract member ShadowHit : r:Ray -> bool * distance:float
    abstract member BoundBox : t0:float * t1:float -> bool * AABB

type INewHitable =
    abstract member Hit : Ray * tMin:float * tMax:float -> NewHitRecord
    abstract member BoundBox : unit -> Bound

type NewAABB(a:Point, b:Point) =
    let pmin = a
    let pmax = b
    static let RIGHT = 0
    static let LEFT = 1
    static let MIDDLE = 2
    member this.min = pmin
    member this.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        // Code rewrite from:
        // https://www.researchgate.net/publication/220494140_An_Efficient_and_Robust_Ray-Box_Intersection_Algorithm
        let origin = r.Origin()
        let dir = r.Direction()
        let tmin, tmax =
            if dir.x >= 0. then
                (this.min.x - origin.x) / dir.x,
                (this.max.x - origin.x) / dir.x
            else
                (this.max.x - origin.x) / dir.x,
                (this.min.x - origin.x) / dir.x
        let tymin, tymax =
            if dir.y >= 0. then
                (this.min.y - origin.y) / dir.y,
                (this.max.y - origin.y) / dir.y
            else
                (this.max.y - origin.y) / dir.y,
                (this.min.y - origin.y) / dir.y
        if tmin > tymax || tymin > tmax then
            false
        else
            let tmin = if tymin > tmin then tymin else tmin
            let tmax = if tymax < tmax then tymax else tmax
            let tzmin, tzmax =
                if dir.z >= 0. then
                    (this.min.z - origin.z) / dir.z,
                    (this.max.z - origin.z) / dir.z
                else
                    (this.max.z - origin.z) / dir.z,
                    (this.min.z - origin.z) / dir.z
            if tmin > tzmax || tzmin > tmax then
                false
            else
                let tmin = if tzmin > tmin then tzmin else tmin
                let tmax = if tzmax < tmax then tzmax else tmax
                tmin < tMax && tmax > tMin
