module Engine.Core.Tracers.RayCast
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld
open Engine.Core.Color
open Engine.Core.Point

type RayCast(world:IWorld) =
    inherit Tracer()
    member this.GetHit(ray, tmin,tmax) =
        world.GetObjects() |>
            Array.map(fun i -> i.Hit(ray,tmin,tmax)) |>
            Array.minBy(fun (hitrecord) ->
                (if hitrecord.bHit then hitrecord.t else tmax))
    override this.TraceRay(ray) = Color()
    override this.TraceRay(ray, depth) =
        let tmin, tmax = 0.000001, 10000000.0
        let hitrecord = world.Hit(ray, tmin, tmax)
        
        //let (_isHit, hitrecord) = this.GetHit(ray, tmin, tmax)
        if hitrecord.bHit then
            let n = hitrecord.normal.Normalize
            let targ = n
            let col = hitrecord.material.Value.Shade(hitrecord, world)
            col
        else
            let unitDirection = ray.Direction().Normalize
            let t = 0.5 * (unitDirection.y + 1.0)
            let vec = (1.0-t)*Vector(1,1,1) + t*Vector(0.5,0.7,1.0)
            Color(vec.x, vec.y, vec.z)