module Engine.Core.Interfaces.HitRecord
open Engine.Core.Point
open Engine.Core.Ray

type HitRecord =
    struct
        val hit: bool
        val t : float
        val point : Point
        val normal : Vector
        val ray : Ray
        val materialIndex : int
        new(t,p,nm,r,m) = {hit=true;t=t;point=p;normal=nm;ray=r;materialIndex=m;}
        static member Empty = HitRecord()
    end