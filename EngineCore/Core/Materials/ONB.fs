module Engine.Core.ONB

open Engine.Core.Point
open System

type ONB =
    struct
        val u:Vector
        val v:Vector
        val w:Vector
        new(n:Vector) =
            let _w = n.Normalize
            let a =
                if Math.Abs(_w.x) > 0.9 then
                    Vector(0,1,0)
                else
                    Vector(1,0,0)
            let _v = _w.Cross(a).Normalize
            let _u = _w.Cross(_v)
            {
                u = _u;
                v = _v;
                w = _w;
            }
        member this.Local(a:Vector) = a.x * this.u + a.y * this.v + a.z * this.w
    end