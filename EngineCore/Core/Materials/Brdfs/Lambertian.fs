module Engine.Core.Materials.Lambertian
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Color

let inv_pi = 1./System.Math.PI
type Lambertian1(kd:float, cd:Color) =
    inherit Brdf()
    override this.f(hit, wi, wo) = kd * cd * inv_pi
    override this.rho(wo,nSamples,samples) = kd*cd