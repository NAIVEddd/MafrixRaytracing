module Engine.Core.Interfaces.ITracer
open Engine.Core.Ray
open Engine.Core.Color

type ITracer =
    abstract TraceRay : Ray -> Color
    abstract TraceRay : Ray * depth:int -> Color

[<AbstractClass>]
type Tracer() =
    abstract TraceRay : Ray -> Color
    abstract TraceRay : Ray * depth:int -> Color
    interface ITracer with
        member this.TraceRay(ray) = this.TraceRay(ray)
        member this.TraceRay(ray,depth) = this.TraceRay(ray,depth)