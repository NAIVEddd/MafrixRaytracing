module Engine.Core.Interfaces.ILight
open Engine.Core.Interfaces.HitRecord
open Engine.Core.Interfaces.IHitable
open Engine.Core.Interfaces.IMaterial
open Engine.Core.Ray
open Engine.Core.Color
open Engine.Core.Point

type ILight =
    abstract GetDirection : HitRecord -> Vector     // Normlized
    abstract L : HitRecord -> Color
    abstract CastsShadows : unit -> bool
    abstract InShadow : HitRecord * Ray * world:IHitable -> bool

type INewLight =
    abstract GetDirection : NewHitRecord -> dist:float * toLight:Vector // Normlized
    abstract L : NewHitRecord * toLight:Vector -> Color

[<AbstractClass>]
type Light() =
    abstract GetDirection : HitRecord -> Vector
    abstract L : HitRecord -> Color
    abstract CastsShadows : unit -> bool
    abstract InShadow : HitRecord * Ray * IHitable -> bool
    override this.CastsShadows() = false
    override this.InShadow(record, ray, world) = false
    interface ILight with
        member this.GetDirection(record) = this.GetDirection(record)
        member this.L(record) = this.L(record)
        member this.CastsShadows() = this.CastsShadows()
        member this.InShadow(record, ray, world) = this.InShadow(record, ray, world)