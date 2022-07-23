module Engine.Core.Interfaces.IBrdf
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Color
open Engine.Core.Point


// type IBrdf =
//     abstract member f : hit:HitRecord * wi:Vector * wo:Vector -> Color
//     abstract member Sample_f : hit:HitRecord * wi:outref<Vector> * wo:Vector * u:float * v:float * pdf:outref<float> -> Color
//     abstract member rho : wo:Vector * nSamples:int * samples:float -> Color

// type Brdf() =
//     abstract member f : hit:HitRecord * wi:Vector * wo:Vector -> Color
//     abstract member Sample_f : hit:HitRecord * wi:outref<Vector> * wo:Vector * u:float * v:float * pdf:outref<float> -> Color
//     abstract member rho : wo:Vector * nSamples:int * samples:float -> Color
//     override this.f(hit, wi, wo) = Color()
//     override this.Sample_f(hit, wi, wo, u, v, pdf) = Color()
//     override this.rho(wo,nSamples,samples) = Color()
//     interface IBrdf with
//         member this.f(hit:HitRecord, wi:Vector, wo:Vector) = this.f(hit,wi,wo)
//         member this.Sample_f(hit,wi,wo,u,v,pdf) = this.Sample_f(hit,ref wi,wo,u,v, ref pdf)
//         member this.rho(wo,nSamples,samples) = this.rho(wo,nSamples,samples)