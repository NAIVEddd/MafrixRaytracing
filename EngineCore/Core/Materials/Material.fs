module Engine.Core.Material
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Ray
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open System

let GetRandomInUnitSphere(nm:Vector) =
    let mutable p = Vector(20,20,20)
    let rand = new System.Random()
    while p.Dot(p) >= 1.0 || nm.Dot(p) <= 0. do
        p <- 2.0 * Vector(rand.NextDouble(), rand.NextDouble(), rand.NextDouble()) - Vector(1,1,1)
    p

let Reflect(v:Vector, n:Vector) = v - 2.0*v.Dot(n)*n
let Refract(v:Vector, n:Vector, ni_over_nt:float) =
    let uv = v.Normalize
    let dt = uv.Dot(n)
    let discriminant = 1.0 - ni_over_nt*ni_over_nt*(1.0-dt*dt)
    if discriminant > 0 then
        (true, ni_over_nt*(v-n*dt) - n*sqrt(discriminant))
    else
        (false, Reflect(v, n))

let INVPI = 1. / Math.PI
type Lambertian(a:Color) =
    let albedo = a
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        assert(abs(1.-hit.normal.Length) < 1e-6)
        let target = (GetRandomInUnitSphere(hit.normal)).Normalize
        //let target = (hit.normal + GetRandomInUnitSphere(hit.normal)).Normalize
        let scattered = Ray(hit.point, target)
        albedo * INVPI, scattered
    member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) =
        let ei = hit.normal.Dot(r.Direction())
        assert(ei >= 0.)
        indirect * 2. * Math.PI * ei
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit,r,indirect) = this.Shade(hit,r,indirect)
        member this.BaseColor() = a
        member this.Emit() = Color()

type Metal(a:Color, f:float) =
    let albedo = a
    let fuzz = if f < 1.0 then f else 1.0
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        let reflected = Reflect(ray.Direction(), hit.normal)
        let dir = (reflected + fuzz*GetRandomInUnitSphere(hit.normal)).Normalize
        let scattered = Ray(hit.point, dir)
        albedo, scattered
    member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = indirect
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit,r,indirect) = this.Shade(hit,r,indirect)
        member this.BaseColor() = a
        member this.Emit() = Color()

type FresnelDielectric(ei:float, et:float) =
    let eta_i = ei
    let eta_t = et
    member private this.frDiel(cosi:float, cost:float, etai:float, etat:float) =
        let rparl = ((etat * cosi) - (etai * cost)) /
                    ((etat * cosi) + (etai * cost))
        let rperp = ((etai * cosi) - (etat * cost)) /
                    ((etai * cosi) + (etat * cost))
        let f = (rparl*rparl + rperp*rperp) / 2.
        Color(f,f,f)
    member this.Evaluate(cosi:float) =
        assert(cosi <= 1.0 && cosi >= -1.0)
        let ei, et =
            if cosi > 0. then
                eta_i, eta_t
            else
                eta_t, eta_i
        let sint = ei / et * sqrt(max 0. (1. - cosi*cosi))
        if sint >= 1. then
            Color(1.0, 1.0, 1.0)
        else
            let cost = sqrt(max 0. (1. - sint*sint))
            this.frDiel(abs(cosi), cost, ei, et)

type SpecularTransmission(t:Color, ei:float, et:float) =
    let T = t
    let etai = ei
    let etat = et
    let fresnel = FresnelDielectric(ei,et)
    member this.Scatter(ray:Ray, hit:NewHitRecord) =
        let dir = -ray.Direction()
        let cosi = dir.Dot(hit.normal)
        let ei, et =
            if cosi > 0. then
                etai, etat
            else
                etat, etai
        let isRefract, refract = Refract(dir, hit.normal, ei/et)
        let color =
            if isRefract then
                let F = fresnel.Evaluate(cosi)
                (et*et)/(ei*ei) * (Color(1,1,1) - F) * T / abs(refract.Dot(hit.normal))
            else
                Color()
        color, Ray(hit.point, refract)
    member this.Shade(hit:NewHitRecord, r:Ray, indirect:Color) = indirect
    interface INewMaterial with
        member this.Scatter(ray:Ray, hit:NewHitRecord) = this.Scatter(ray,hit)
        member this.Shade(hit,r,indirect) = this.Shade(hit,r,indirect)
        member this.BaseColor() = T
        member this.Emit() = Color()