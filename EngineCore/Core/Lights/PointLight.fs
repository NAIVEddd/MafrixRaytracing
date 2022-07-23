module Engine.Core.Lights.PointLight
open Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Ray
open Engine.Core.Color
open Engine.Core.Point

// type PointLight(ls:float,color:Color,location:Point) =
//     inherit Light()
//     override this.GetDirection(record) = (location - record.p).Normalize
//     override this.L(record) = ls*color
//     override this.CastsShadows() = true
//     override this.InShadow(hit:HitRecord, ray:Ray, world:IHitable) =
//         let d = (location - ray.Origin()).Length
//         let bHit, tmin = world.ShadowHit(ray)
//         if bHit && tmin < d then
//             true
//         else
//             false