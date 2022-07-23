module Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Aggregate
open Engine.Core.Ray
open Engine.Core.Point

type IHitable =
    abstract member Hit : Ray * tMin:float * tMax:float -> HitRecord
    abstract member BoundBox : unit -> Bound

[<Struct>]
type AABB(pmin:Point, pmax:Point) =
    static let RIGHT = 0
    static let LEFT = 1
    static let MIDDLE = 2
    member _.min = pmin
    member _.max = pmax
    member this.hit(r:Ray, tMin:float, tMax:float) =
        // Code rewrite from:
        // https://www.researchgate.net/publication/220494140_An_Efficient_and_Robust_Ray-Box_Intersection_Algorithm
        let origin = r.Origin()
        let dir = r.Direction()
        let tmin, tmax =
            if dir.x >= 0. then
                (pmin.x - origin.x) / dir.x,
                (pmax.x - origin.x) / dir.x
            else
                (pmax.x - origin.x) / dir.x,
                (pmin.x - origin.x) / dir.x
        let tymin, tymax =
            if dir.y >= 0. then
                (pmin.y - origin.y) / dir.y,
                (pmax.y - origin.y) / dir.y
            else
                (pmax.y - origin.y) / dir.y,
                (pmin.y - origin.y) / dir.y
        if tmin > tymax || tymin > tmax then
            false
        else
            let tmin = if tymin > tmin then tymin else tmin
            let tmax = if tymax < tmax then tymax else tmax
            let tzmin, tzmax =
                if dir.z >= 0. then
                    (pmin.z - origin.z) / dir.z,
                    (pmax.z - origin.z) / dir.z
                else
                    (pmax.z - origin.z) / dir.z,
                    (pmin.z - origin.z) / dir.z
            if tmin > tzmax || tzmin > tmax then
                false
            else
                let tmin = if tzmin > tmin then tzmin else tmin
                let tmax = if tzmax < tmax then tzmax else tmax
                tmin < tMax && tmax > tMin
    static member surroundingBox(box0:AABB, box1:AABB) =
        let small = Point(min box0.min.x box1.min.x, min box0.min.y box1.min.y, min box0.min.z box1.min.z)
        let big = Point(max box0.max.x box1.max.x, max box0.max.y box1.max.y, max box0.max.z box1.max.z)
        AABB(small,big)