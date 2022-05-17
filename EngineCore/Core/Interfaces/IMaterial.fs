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

type INewMaterial =
    abstract member Scatter : ray:Ray * NewHitRecord -> Color * Ray
    abstract member Shade : NewHitRecord * scatteredRay:Ray * indirectLight:Color -> Color
    abstract member BaseColor : unit -> Color
    abstract member Emit : unit -> Color

type MaterialManager() as this =
    do
        this.materials<-Array.empty
    [<DefaultValue>]
    val mutable materials : INewMaterial[]
    static let DefaultManager = MaterialManager()
    static member GetManager() = DefaultManager

    member this.Item
        with get(i) = this.materials[i]
    member this.Add(mat:INewMaterial) = this.materials <- Array.insertAt this.materials.Length mat this.materials
    member this.Add(mats:INewMaterial seq) = this.materials <- Array.insertManyAt this.materials.Length mats this.materials
