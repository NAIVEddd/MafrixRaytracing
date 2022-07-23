module Engine.Core.Lights.AreaLight
open System
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.ISampler
open Engine.Core.Samplers.JitteredSampler
open Engine.Core.Ray
open Engine.Core.Color
open Engine.Core.Point

type SphereSampler(location:Point, radius:float, nSamples:int) =
    let sampler = JitteredSampler(nSamples)
    member this.SampTo3D(p:Point2D) =
        let phi = p.x * 360.
        let theta = p.y * 360.
        let p = phi * (Math.PI / 180.0)
        let t = theta * (Math.PI / 180.0)
        let sin_phi = sin p
        let cos_phi = cos p
        let sin_theta = sin t
        let cos_theta = cos t
        let x = cos_phi * sin_theta
        let y = sin_phi
        let z = cos_phi * cos_theta
        location + radius * Vector(x,y,z)
    member this.Sample() =
        let p = sampler.SampleUnitSquare()
        this.SampTo3D(p)

// type AreaLight(ls:float, radius:float,color:Color,location:Point,dir:Vector,obj:IHitable,world:IHitable) =
//     inherit Light()
//     let sampler = SphereSampler(location, radius, 64)
//     let mutable point = Point()
//     member this.G(hit:HitRecord) = 0.0
//     member this.PDF(hit:HitRecord) = 1.0/64.0
//     override this.GetDirection(record) =
//         point <- sampler.Sample()
//         (point - record.p).Normalize
//     override this.L(record) =
//         //let color =
//         //    [|0..63|] |> Array.map(fun(n) ->
//         //            let p = sampler.Sample()
//         //            let d = (p - record.p).Length
//         //            let r = Ray(record.p, (p - record.p).Normalize)
//         //            let bHit, tmin = world.ShadowHit(r)
//         //            if bHit && tmin < d then
//         //                true
//         //            else
//         //                false)
//         //        |> Array.map(fun(inshadow) ->
//         //            if inshadow then
//         //                Color()
//         //            else
//         //                ls*color)
//         //        |> Array.fold (fun l r -> l+r) (Color())
//         //color / 64.0
//         ls*color
        
//     override this.CastsShadows() = true
//     override this.InShadow(hit:HitRecord, ray:Ray, world:IHitable) =
//         //[|0..63|] |> Array.forall(fun(n) ->
//         //    let p = sampler.Sample()
//         //    let d = (p - ray.Origin()).Length
//         //    let r = Ray(ray.Origin(), (p - ray.Origin()).Normalize)
//         //    let bHit, tmin = world.ShadowHit(r)
//         //    if bHit && tmin < d then
//         //        true
//         //    else
//         //        false)
//         let d = (point - ray.Origin()).Length
//         //let r = Ray(ray.Origin(), (point - ray.Origin()).Normalize)
//         let bHit, tmin = world.ShadowHit(ray)
//         if bHit && tmin < d then
//             true
//         else
//             false
        
