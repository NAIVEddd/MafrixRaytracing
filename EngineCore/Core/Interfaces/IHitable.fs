module Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Ray

type IHitable =
    abstract member Hit : r:Ray * tMin:float * tMax:float -> bool * HitRecordT<IMaterial>
    abstract member ShadowHit : r:Ray -> bool * distance:float