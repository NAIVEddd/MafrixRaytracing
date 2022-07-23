module Engine.Core.Tracers.PathTracer
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.ITracer
open Engine.Core.Interfaces.IWorld
open Engine.Core.Accels.BvhNode
open Engine.Core.Ray
open Engine.Core.Color
open Engine.Core.Point

type NewPathTracer(bvh:Bvh, maxDepth, light:INewLight) =
    member this.VisibilityTest(hit:HitRecord) =
        let dist,toLight = light.GetDirection(hit)
        let unitToLight = toLight/dist
        let shadowHit = bvh.Hit(Ray(hit.point,unitToLight),1e-6,dist-1e-6)
        if shadowHit.hit then
            Color()
        else
            let l = light.L(hit,toLight)
            unitToLight.Dot(hit.normal) * l
    member this.TraceRay(ray:Ray, depth:int) =
        let hit = bvh.Hit(ray,1e-6,99999999.)
        if hit.hit && depth >= 0 then
            let material = MaterialManager.GetManager()[hit.materialIndex]
            //if hit.materialIndex = 5 then
            //    material.BaseColor()
            //else
            //    let col, r = material.Scatter(ray, hit)
            //    let t = this.TraceRay(r, depth - 1)
            //    let shade = material.Shade(hit,r,t)
            //    let l = this.VisibilityTest(hit)
            //    l * col // direct light
            //        + col * shade
            let col, r = material.Scatter(ray, hit)
            let t = this.TraceRay(r, depth - 1)
            let shade = material.Shade(hit,r,t)
            let l = this.VisibilityTest(hit)
            l * col // direct light
                + col * shade
        else
            Color()
    member this.Trace(ray:Ray) = this.TraceRay(ray, maxDepth)
    interface INewTracer with
        member this.TraceRay(ray) = this.Trace(ray)

// type PathTracer(world:IWorld, maxDepth) =
//     inherit Tracer()
//     member this.GetHit(ray, tmin,tmax) =
//         world.GetObjects() |>
//             Array.map(fun i -> i.Hit(ray,tmin,tmax)) |>
//             Array.minBy(fun hitrecord ->
//                 (if hitrecord.bHit then hitrecord.t else tmax))
//     override this.TraceRay(ray) = Color()
//     override this.TraceRay(ray, depth) =
//         let tmin, tmax = 0.000001, 10000000.0
//         let hitrecord = world.Hit(ray, tmin, tmax)
//         if hitrecord.bHit && depth < maxDepth then
//             let n = hitrecord.normal.Normalize
//             let targ = n// + GetRandomInUnitSphere()
//             hitrecord.material.Value.PathShade(hitrecord,world,depth)
//         else
//             let unitDirection = ray.Direction().Normalize
//             let t = 0.5 * (unitDirection.y + 1.0)
//             let vec = (1.0-t)*Vector(1,1,1) + t*Vector(0.5,0.7,1.0)
//             Color()
