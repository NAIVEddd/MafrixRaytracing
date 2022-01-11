module Engine.Core.Materials.PerfectSpecular
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Color

type PerfectSpecular(kr:float, col:Color) =
    inherit Brdf()
    override this.Sample_f(hit, wi, wo, u, v, pdf) =
        let ndotwo = hit.normal.Dot(wo)
        wi <- -wo + 2.0 * hit.normal * ndotwo
        kr * col / (hit.normal.Dot(wi))
    override this.f(hit, wi, wo) =
        Color(0,0,0)
    override this.rho(wo,nSamples,samples) = Color()
