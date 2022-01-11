module Engine.Core.Ray
open Engine.Core.Point

[<Struct>]
type Ray(origin:Point, direction:Vector) =
    member this.Origin() = origin
    member this.Direction() = direction
    member this.PointAtParameter(t:float) = 
        assert(abs(1.0 - direction.Length) < 0.00001)
        origin + direction*t