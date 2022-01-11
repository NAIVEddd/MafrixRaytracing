module Engine.Core.Materials.GlossySpecular
open Engine.Core.Interfaces.IBrdf
open Engine.Core.Color

type GlossySpecular(ks:float, exp:float, col:Color) =
    inherit Brdf()
    override this.f(hit, wi, wo) =
        let ndotwi = hit.normal.Dot(wi)
        let r = -wi + 2.0*ndotwi*hit.normal
        let rdotwo = r.Dot(wo)
        if rdotwo > 0.0 then
            ks * System.Math.Pow(rdotwo, exp) * col
        else
            Color()
    override this.rho(wo,nSamples,samples) = ks*col