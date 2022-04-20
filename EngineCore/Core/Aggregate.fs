module Engine.Core.Aggregate
open Engine.Core.Point
open Engine.Core.Ray

type Bound =
    struct
        val pMin:Point
        val pMax:Point
        new(p1:Point, p2:Point) =
            {pMin=Point(min p1.x p2.x,min p1.y p2.y,min p1.z p2.z);
             pMax=Point(max p1.x p2.x,max p1.y p2.y,max p1.z p2.z);}
        member this.Item
            with get(idx) =
                match idx with
                | 0 -> this.pMin
                | 1 -> this.pMax
                | _ -> assert(false);Point()
        member this.Corner(corner:int) =
            Point(this[corner &&& 1].x,
                  this[if corner &&& 2 = 0 then 0 else 1].y,
                  this[if corner &&& 4 = 0 then 0 else 1].z)
        member this.Diagnal() = this[1] - this[0]
        member this.SurfaceArea() =
            let d = this.Diagnal()
            2. * (d.x*d.y + d.x*d.z + d.y*d.z)
        member this.Volume() =
            let d = this.Diagnal()
            d.x * d.y * d.z
        member this.MaximumExtent() =
            let d = this.Diagnal()
            if d.x > d.y && d.x > d.z then // x max
                0
            elif d.y > d.z then     // y max
                1
            else        // z max
                2
        member this.Lerp(t:Point) =
            Point(Lerp(t.x,this.pMin.x, this.pMax.x),
                  Lerp(t.y,this.pMin.y, this.pMax.y),
                  Lerp(t.z,this.pMin.z, this.pMax.z))
        member this.Offset(p:Point) =
            let d = this.Diagnal()
            let o = p - this.pMin
            let x = if this.pMax.x > this.pMin.x then o.x / d.x else o.x
            let y = if this.pMax.y > this.pMin.y then o.y / d.y else o.y
            let z = if this.pMax.z > this.pMin.z then o.z / d.z else o.z
            Vector(x,y,z)
        member this.BoundingSphere() =
            let d = this.Diagnal()
            let center = this.pMin + d / 2.0
            let radius = if Bound.Inside(center, this) then (this.pMax-center).Length else 0.
            center, radius
        static member DefaultValue =
            let p1 = System.Double.NegativeInfinity
            let p2 = System.Double.PositiveInfinity
            Bound(Point(p1,p1,p1),Point(p2,p2,p2))
        static member Union(b1:Bound, p:Point) =
            let p1 = Point(min b1.pMin.x p.x, min b1.pMin.y p.y, min b1.pMin.z p.z)
            let p2 = Point(max b1.pMax.x p.x, max b1.pMax.y p.y, max b1.pMax.z p.z)
            Bound(p1,p2)
        static member Union(b1:Bound, b2:Bound) =
            let p1 = Point(min b1.pMin.x b2.pMin.x, min b1.pMin.y b2.pMin.y, min b1.pMin.z b2.pMin.z)
            let p2 = Point(max b1.pMax.x b2.pMax.x, max b1.pMax.y b2.pMax.y, max b1.pMax.z b2.pMax.z)
            Bound(p1,p2)
        static member Intersect(b1:Bound, b2:Bound) =
            let p1 = Point(max b1.pMin.x b2.pMin.x, max b1.pMin.y b2.pMin.y, max b1.pMin.z b2.pMin.z)
            let p2 = Point(min b1.pMax.x b2.pMax.x, min b1.pMax.y b2.pMax.y, min b1.pMax.z b2.pMax.z)
            Bound(p1,p2)
        static member Overlaps(b1:Bound, b2:Bound) =
            let x = b1.pMax.x >= b2.pMin.x && b1.pMin.x <= b2.pMax.x
            let y = b1.pMax.y >= b2.pMin.x && b1.pMin.y <= b2.pMax.y
            let z = b1.pMax.z >= b2.pMin.z && b1.pMin.z <= b2.pMax.z
            x && y && z
        static member Inside(p:Point, b:Bound) =
            p.x >= b.pMin.x && p.x <= b.pMax.x &&
            p.y >= b.pMin.y && p.y <= b.pMax.y &&
            p.z >= b.pMin.z && p.z <= b.pMax.z
        // not include uper bound
        static member InsideExclusive(p:Point, b:Bound) =
            p.x >= b.pMin.x && p.x < b.pMax.x &&
            p.y >= b.pMin.y && p.y < b.pMax.y &&
            p.z >= b.pMin.z && p.z < b.pMax.z
        static member Expand(b:Bound, delta:float) =
            let delt = Vector(delta,delta,delta)
            Bound(b.pMin - delt, b.pMax + delt)
    end

type Primitive =
    abstract member WorldBound: unit -> Bound
    abstract member IntersectP: Ray -> bool
