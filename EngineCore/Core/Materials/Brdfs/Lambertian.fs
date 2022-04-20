module Engine.Core.Materials.Lambertian
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.ONB
open Engine.Core.Samplers.JitteredSampler
open System

let inv_pi = 1./System.Math.PI
let GetRandomInUnitSphere() =
    let mutable p = Vector(20,20,20)
    let rand = new System.Random()
    while p.Dot(p) >= 1.0 do
        p <- 2.0 * Vector(rand.NextDouble(), rand.NextDouble(), rand.NextDouble()) - Vector(1,1,1)
    p

let GetRandomInHemisphere(normal:Vector) =
    let rand = System.Random()
    let mutable p = GetRandomInUnitSphere()
    while normal.Dot(p) < 0. do
        p <- GetRandomInUnitSphere()
    p.Normalize

let GetRandomCosineDirection() =
    let rand = Random.Shared
    let r1 = rand.NextDouble()
    let r2 = rand.NextDouble()
    let z = Math.Sqrt(1.-r2)
    let phi = 2. * Math.PI * r1
    let x = Math.Cos(phi) * 2. * Math.Sqrt(r2)
    let y = Math.Sin(phi) * 2. * Math.Sqrt(r2)
    Vector(x,y,z).Normalize

let GetAoCosineDirection() =
    let rand = Random.Shared
    let r1 = rand.NextDouble()
    let r2 = rand.NextDouble()
    let x = Math.Sqrt(r1) * Math.Cos(2. * Math.PI * r2)
    let y = Math.Sqrt(r1) * Math.Sin(2. * Math.PI * r2)
    let z = Math.Sqrt(1. - r1)
    Vector(x,y,z)

let GetHemisphereOrientedToNormal(nm:Vector) =
    let rand = Random.Shared
    let r1 = rand.NextDouble()
    let r2 = rand.NextDouble()
    let a = 1. - 2. * r1
    let b = Math.Sqrt(1. - a*a)
    let phi = 2. * Math.PI * r2
    let x = nm.x + b * Math.Cos(phi)
    let y = nm.y + b * Math.Sin(phi)
    let z = nm.z + a
    Vector(x,y,z).Normalize, a / Math.PI

type Lambertian1(kd:float, cd:Color) =
    inherit Brdf()
    let sampler = JitteredSampler(256)
    do
        sampler.GenerateSamples()
    override this.Sample_f(hit, wi, wo, u, v, pdf) =
        let w = hit.normal
        let v = Vector(0.0034, 1.0, 0.0071).Cross(w).Normalize
        let u = v.Cross(w)
        let sp = sampler.SampleHemisphere()
        //let _wi = sp.x * u + sp.y * v + sp.z * w
        //wi <- _wi.Normalize
        let uvw = ONB(w)
        //wi <- uvw.Local(GetAoCosineDirection())
        //pdf <- hit.normal.Dot(wi) * inv_pi
        let twi, tpdf = GetHemisphereOrientedToNormal(w)
        wi <- twi
        pdf <- tpdf
        //kd * cd * inv_pi
        kd * cd * pdf

    override this.f(hit, wi, wo) = kd * cd * inv_pi
    override this.rho(wo,nSamples,samples) = kd*cd