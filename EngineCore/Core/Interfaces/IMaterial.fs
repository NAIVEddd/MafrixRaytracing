module Engine.Core.Interfaces.IMaterial
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Color
open Engine.Core.Point
open Engine.Core.Ray

type IMaterial =
    abstract member Scatter : ray:Ray * HitRecord -> bool * attenuation:Vector * Ray
    abstract member Shade: HitRecord * world:obj * depth:int * wo:outref<Vector> -> Color
    abstract member Shade: HitRecord * world:obj -> Color
    abstract member PathShade: HitRecord * world:obj * depth:int -> Color

and HitRecord = HitRecordT<IMaterial>