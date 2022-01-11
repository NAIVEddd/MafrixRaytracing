module Engine.Core.Interfaces.HitRecord
open Engine.Core.Point
open Engine.Core.Ray

// 'M used as material
type HitRecordT<'M> =
    struct
        val bHit: bool
        val t: float
        val p: Point
        val normal: Vector
        val hitRay: Ray
        val material: Option<'M>
        new(_bHit, _t:float, _p:Point, _nm:Vector, _ray:Ray, mate:Option<'M>) = {bHit = _bHit; t = _t; p = _p; normal = _nm; hitRay = _ray; material = mate}
        static member Nothing = HitRecordT<'M>(false, -1,Point(),Vector(), Ray(), None)
    end